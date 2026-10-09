# =============================================================================
# Évaluation du REME (Strasbourg) — Script 01 : jointure des bases
# -----------------------------------------------------------------------------
# Lit data/raw/ (produit par le script 00) et table_traitement_lignes.csv.
# Écrit dans DIR_DONNEES/intermediaire/ :
#   gares.csv                    une ligne par gare du Grand Est : UIC, commune,
#                                lignes RFN, segment, statut de traitement, exclusion
#   gares_segments.csv           toutes les appartenances gare × segment (contrôle
#                                des gares situées en bout de deux segments)
#   covariables_communes.csv     RP 2019 logement, Filosofi 2021, MOBPRO 2019 / 2022,
#                                en géographie communale courante
#   covariables_bassins.csv      les mêmes, agrégées sur le bassin de chaque gare
#   panel_gares.csv              gare × année : fréquentation, traitement, covariables
#   panel_stations_routieres.csv station × année : TMJA, gare et segment les plus proches
#   rapport_jointures.csv        taux d'appariement de chaque jointure
#
# Clés de jointure :
#   fréquentation ─ UIC ─ gares RFN ─ code ligne ─ lignes RFN ─ libellé ─ table_traitement
#                          │ coordonnées
#                          └─ commune (contours) ─ code commune ─ covariables Insee
#                                                   (via la table de passage)
#   comptages routiers ─ distance ─ gare la plus proche
#
# Le script joint, il ne choisit pas l'échantillon : les gares à exclure sont
# signalées (colonne exclusion), pas supprimées.
#
# Les noms de colonnes des fichiers sources n'ont pas pu être vérifiés sur les
# fichiers réels. Chaque colonne est donc cherchée par motif ; si elle manque,
# le script s'arrête en listant les colonnes disponibles : corriger le motif.
#
# Dépendances : data.table, sf, stringi, readxl, jsonlite.
# =============================================================================

library(data.table)
library(sf)

# ---- 0. Paramètres ----------------------------------------------------------

DIR_DONNEES        <- "data"
FICHIER_TRAITEMENT <- "table_traitement_lignes.csv"

DEP_GRAND_EST  <- c("08", "10", "51", "52", "54", "55", "57", "67", "68", "88")
EPCI_EMS       <- "246700488"   # SIREN de l'Eurométropole de Strasbourg
RAYON_BASSIN_KM <- 10           # une commune rejoint le bassin de sa gare la plus proche si elle est à moins de R km

# Géographie des sources (année du code officiel géographique utilisé par l'Insee)
GEO_RP2019      <- 2021
GEO_MOBPRO2019  <- 2021
GEO_MOBPRO2022  <- 2024
GEO_FILOSOFI2021 <- 2022        # à vérifier dans la documentation du fichier

# Quand une gare appartient à plusieurs segments, le premier statut de cette
# liste l'emporte. « exclue » passe avant les témoins : une gare touchée par un
# autre changement d'offre ne doit pas servir de témoin.
PRIORITE_STATUT <- c("traitee", "traitee_faible", "reduite", "exclue", "a_verifier", "temoin_proche")

# Gares à exclure de l'estimation (motif sur le nom normalisé : minuscules, sans accents)
GARES_EXCLUES <- data.frame(
  motif  = c("^strasbourg-ville$|^strasbourg$", "^lauterbourg$", "^sarreguemines$", "^saint-die"),
  raison = c("destination (TGV, international, TER)", "transfrontalier",
             "pôle desservi par d'autres lignes", "pôle desservi par d'autres lignes"),
  stringsAsFactors = FALSE)

# ---- 1. Fonctions -----------------------------------------------------------

dir_raw <- file.path(DIR_DONNEES, "raw")
dir_out <- file.path(DIR_DONNEES, "intermediaire")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# minuscules sans accents ; les fichiers Insee anciens sont parfois en latin-1
norm_txt <- function(x) {
  x <- as.character(x)
  mauvais <- !is.na(x) & !validUTF8(x)
  x[mauvais] <- iconv(x[mauvais], "latin1", "UTF-8")
  tolower(stringi::stri_trans_general(trimws(x), "Latin-ASCII"))
}
norm_noms <- function(x) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", norm_txt(x)))

lire <- function(f, ...) {
  if (!file.exists(f)) stop("Fichier absent : ", f, " (relancer le script 00)", call. = FALSE)
  fread(f, colClasses = "character", encoding = "UTF-8", ...)
}

chercher_fichier <- function(dossier, motif) {
  f <- list.files(file.path(dir_raw, dossier), motif, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(f)) f[1] else NA_character_
}

# première colonne dont le nom normalisé correspond au motif
col <- function(dt, motif, quoi, obligatoire = TRUE) {
  n <- names(dt)[grepl(motif, norm_noms(names(dt)))]
  if (length(n)) return(n[1])
  if (obligatoire)
    stop(sprintf("%s : aucune colonne ne correspond à « %s ».\nColonnes disponibles : %s",
                 quoi, motif, paste(names(dt), collapse = " | ")), call. = FALSE)
  NA_character_
}

# Code UIC sur 7 chiffres (87 + 5 chiffres de gare) : certains fichiers ajoutent
# la clé de contrôle (8e chiffre), d'autres non. On joint sans la clé.
norm_uic <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  ifelse(nchar(x) %in% 7:8, substr(x, 1, 7), NA_character_)
}

# Code ligne RFN sur 6 chiffres (les zéros de tête disparaissent dans certains fichiers)
norm_ligne <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & nzchar(x)
  out[ok] <- sprintf("%06d", as.integer(x[ok]))
  out
}

# Point kilométrique « 502+584 » -> 502,584 km
lire_pk <- function(x) {
  x <- gsub("\\s", "", as.character(x))
  suppressWarnings(ifelse(grepl("\\+", x),
    as.numeric(sub("\\+.*", "", x)) + as.numeric(sub(".*\\+", "", x)) / 1000,
    as.numeric(sub(",", ".", x))))
}

# Coordonnées : colonne « lat, lon » des exports Opendatasoft, sinon deux colonnes
coordonnees <- function(dt, quoi) {
  motif_point <- "^\\s*-?\\d+\\.\\d+\\s*,\\s*-?\\d+\\.\\d+\\s*$"
  for (n in names(dt)) {
    v <- head(dt[[n]][!is.na(dt[[n]]) & nzchar(dt[[n]])], 50)
    if (length(v) && all(grepl(motif_point, v))) {
      p <- tstrsplit(dt[[n]], ",")
      return(data.table(lat = as.numeric(p[[1]]), lon = as.numeric(p[[2]])))
    }
  }
  cx <- col(dt, "longitude|x_wgs|wgs_?84_x|^lon$", quoi, FALSE)
  cy <- col(dt, "latitude|y_wgs|wgs_?84_y|^lat$", quoi, FALSE)
  if (is.na(cx) || is.na(cy)) stop(quoi, " : coordonnées introuvables", call. = FALSE)
  data.table(lat = as.numeric(dt[[cy]]), lon = as.numeric(dt[[cx]]))
}

nombre <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", x))))

rapport <- list()
noter <- function(jointure, n_total, n_apparies, remarque = "") {
  rapport[[length(rapport) + 1]] <<- data.table(
    jointure = jointure, n_total = n_total, n_apparies = n_apparies,
    part_appariee = round(n_apparies / max(n_total, 1), 3), remarque = remarque)
  message(sprintf("  %-45s %5d / %-5d (%.0f %%) %s", jointure, n_apparies, n_total,
                  100 * n_apparies / max(n_total, 1), remarque))
}

ecrire <- function(dt, nom) {
  fwrite(dt, file.path(dir_out, nom))
  message(sprintf("  -> %s (%d lignes)", nom, nrow(dt)))
}

# ---- 2. Réseau : gares RFN, lignes RFN --------------------------------------

message("\n=== Réseau ===")

rfn <- lire(file.path(dir_raw, "reseau", "rfn_liste_des_gares.csv"))
xy  <- coordonnees(rfn, "liste des gares RFN")
gares_rfn <- data.table(
  uic7       = norm_uic(rfn[[col(rfn, "uic", "gares RFN")]]),
  nom_gare   = rfn[[col(rfn, "^libelle|^nom", "gares RFN")]],
  code_ligne = norm_ligne(rfn[[col(rfn, "code_ligne", "gares RFN")]]),
  pk         = lire_pk(rfn[[col(rfn, "^pk", "gares RFN")]]),
  voyageurs  = norm_txt(rfn[[col(rfn, "voyageur", "gares RFN")]]),
  lon = xy$lon, lat = xy$lat)
gares_rfn <- gares_rfn[grepl("^(o|oui|y|true|1)", voyageurs) & !is.na(uic7)][, voyageurs := NULL]
gares_rfn[, nom_norm := norm_txt(nom_gare)]

lig <- lire(file.path(dir_raw, "reseau", "rfn_lignes_par_statut.csv"))
lignes_rfn <- unique(data.table(
  code_ligne = norm_ligne(lig[[col(lig, "code_ligne", "lignes RFN")]]),
  lib_ligne  = lig[[col(lig, "^libelle|^lib_ligne|^nom", "lignes RFN")]]))
lignes_rfn[, lib_norm := norm_txt(lib_ligne)]

noter("gares RFN -> lignes RFN (code ligne)", uniqueN(gares_rfn$code_ligne),
      sum(unique(gares_rfn$code_ligne) %in% lignes_rfn$code_ligne))

# ---- 3. Segments de traitement -> gares --------------------------------------
# Pour chaque segment : lignes dont le libellé correspond au motif, puis, si des
# gares de début et de fin sont données, gares dont le PK est compris entre les
# leurs sur la même ligne.

message("\n=== Segments de traitement ===")

trait <- fread(FICHIER_TRAITEMENT, sep = ";", colClasses = "character", encoding = "UTF-8")
for (v in c("gare_debut", "gare_fin", "annees_perturbees")) trait[is.na(get(v)), (v) := ""]
trait[, `:=`(ordre = .I, variation_offre_semaine_pct = nombre(variation_offre_semaine_pct))]
statuts_inconnus <- setdiff(trait$statut, PRIORITE_STATUT)
if (length(statuts_inconnus)) stop("Statuts absents de PRIORITE_STATUT : ", paste(statuts_inconnus, collapse = ", "))

gares_segment <- function(s) {
  codes <- lignes_rfn[grepl(s$motif_ligne_rfn, lib_norm), unique(code_ligne)]
  if (!length(codes)) {
    message(sprintf("  [alerte] %s : aucune ligne RFN ne correspond à « %s »", s$segment_id, s$motif_ligne_rfn))
    return(NULL)
  }
  g <- gares_rfn[code_ligne %in% codes]
  if (nzchar(s$gare_debut) && nzchar(s$gare_fin)) {
    g <- g[, {
      d <- pk[grepl(s$gare_debut, nom_norm)]
      f <- pk[grepl(s$gare_fin, nom_norm)]
      if (length(d) && length(f)) .SD[pk >= min(d, f, na.rm = TRUE) & pk <= max(d, f, na.rm = TRUE)] else .SD[0]
    }, by = code_ligne]
  } else if (nzchar(s$gare_debut) || nzchar(s$gare_fin)) {
    message(sprintf("  [alerte] %s : une seule borne renseignée, toute la ligne est retenue", s$segment_id))
  }
  if (!nrow(g)) {
    message(sprintf("  [alerte] %s : aucune gare (bornes introuvables sur les lignes %s ?)",
                    s$segment_id, paste(codes, collapse = ", ")))
    return(NULL)
  }
  message(sprintf("  %s %-45s %3d gares, ligne(s) %s", s$segment_id, s$ligne_service,
                  uniqueN(g$uic7), paste(unique(g$code_ligne), collapse = ", ")))
  unique(g[, .(uic7, nom_gare, code_ligne, segment_id = s$segment_id)])
}

appartenances <- rbindlist(lapply(seq_len(nrow(trait)), \(i) gares_segment(trait[i])))
appartenances <- merge(appartenances,
                       trait[, .(segment_id, ligne_service, statut, variation_offre_semaine_pct, annees_perturbees, ordre)],
                       by = "segment_id")
appartenances[, rang_statut := match(statut, PRIORITE_STATUT)]
setorder(appartenances, uic7, rang_statut, ordre)

choix_segment <- appartenances[, .(
  segment_id = segment_id[1], ligne_service = ligne_service[1], statut = statut[1],
  variation_offre_semaine_pct = variation_offre_semaine_pct[1],
  annees_perturbees = annees_perturbees[1],
  n_segments = uniqueN(segment_id),
  tous_segments = paste(unique(segment_id), collapse = "|"),
  tous_statuts  = paste(unique(statut), collapse = "|")), by = uic7]

multi <- choix_segment[n_segments > 1]
if (nrow(multi)) {
  message("  Gares dans plusieurs segments (statut retenu selon PRIORITE_STATUT) :")
  print(merge(multi, unique(gares_rfn[, .(uic7, nom_gare)]), by = "uic7")[
    , .(nom_gare, tous_segments, tous_statuts, statut)], row.names = FALSE)
}

# ---- 4. Gares : commune, référentiel voyageurs, exclusions ------------------

message("\n=== Gares ===")

gares <- gares_rfn[, .(nom_gare = nom_gare[1], nom_norm = nom_norm[1],
                       lignes_rfn = paste(sort(unique(code_ligne)), collapse = "|"),
                       lon = mean(lon, na.rm = TRUE), lat = mean(lat, na.rm = TRUE)), by = uic7]

# Commune par position (géographie courante de l'API Géo) plutôt que par le code
# commune des fichiers SNCF, dont le millésime n'est pas documenté.
contours <- st_read(file.path(dir_raw, "geographie", "communes_grand_est_contours.geojson"), quiet = TRUE)
contours <- st_make_valid(contours[, c("code", "nom", "codeEpci", "codeDepartement")])
names(contours)[1:4] <- c("code_commune", "nom_commune", "epci", "dep")
gares_sf <- st_as_sf(gares[!is.na(lon) & !is.na(lat)], coords = c("lon", "lat"), crs = 4326, remove = FALSE)
gares_sf <- st_join(gares_sf, st_transform(contours, 4326), join = st_intersects, left = FALSE)
gares <- as.data.table(st_drop_geometry(gares_sf))
gares <- gares[!duplicated(uic7)]          # gare posée sur une limite communale
noter("gares RFN voyageurs -> commune du Grand Est", nrow(gares_rfn[!duplicated(uic7)]), nrow(gares),
      "les gares hors Grand Est sortent ici")

# Référentiel des gares de voyageurs (Gares & Connexions) : segment DRG
f_gv <- file.path(dir_raw, "reseau", "gares_de_voyageurs.csv")
if (file.exists(f_gv)) {
  gv <- lire(f_gv)
  c_uic <- col(gv, "uic", "gares de voyageurs")
  c_drg <- col(gv, "drg", "gares de voyageurs", obligatoire = FALSE)
  codes <- strsplit(gv[[c_uic]], "[;,|/ ]+")          # une gare peut porter plusieurs codes UIC
  gv <- data.table(uic7 = norm_uic(unlist(codes)),
                   segment_drg = rep(if (is.na(c_drg)) NA_character_ else gv[[c_drg]], lengths(codes)))
  gv <- unique(gv[!is.na(uic7)], by = "uic7")
  gares <- merge(gares, gv, by = "uic7", all.x = TRUE)
  noter("gares -> référentiel gares de voyageurs (UIC)", nrow(gares), sum(gares$uic7 %in% gv$uic7))
}

gares <- merge(gares, choix_segment, by = "uic7", all.x = TRUE)
gares[is.na(statut), `:=`(statut = "hors_segment", n_segments = 0L)]
gares[, exclusion := NA_character_]
for (i in seq_len(nrow(GARES_EXCLUES)))
  gares[is.na(exclusion) & grepl(GARES_EXCLUES$motif[i], nom_norm), exclusion := GARES_EXCLUES$raison[i]]

message("  Gares du Grand Est par statut :")
print(gares[, .N, by = .(statut, exclue = !is.na(exclusion))][order(statut)], row.names = FALSE)

# ---- 5. Fréquentation -> gares ----------------------------------------------

message("\n=== Fréquentation ===")

freq <- lire(file.path(dir_raw, "resultat", "frequentation_gares.csv"))
c_uic  <- col(freq, "uic", "fréquentation")
c_cp   <- col(freq, "code_postal", "fréquentation", obligatoire = FALSE)
c_ans  <- names(freq)[grepl("^total_voyageurs_\\d{4}$", norm_noms(names(freq)))]   # hors « + non voyageurs »
if (!length(c_ans)) stop("fréquentation : aucune colonne « Total Voyageurs AAAA »")

freq_long <- melt(freq[, c(c_uic, c_ans), with = FALSE], id.vars = c_uic,
                  variable.name = "variable", value.name = "voyageurs", variable.factor = FALSE)
freq_long <- freq_long[, .(uic7 = norm_uic(get(c_uic)),
                           annee = as.integer(sub(".*_(\\d{4})$", "\\1", norm_noms(variable))),
                           voyageurs = nombre(voyageurs))]
doublons <- freq_long[, .N, by = .(uic7, annee)][N > 1, uniqueN(uic7)]
if (doublons) message(sprintf("  [alerte] %d codes UIC en double dans la fréquentation : valeurs additionnées", doublons))
freq_long <- freq_long[!is.na(uic7), .(voyageurs = sum(voyageurs, na.rm = TRUE)), by = .(uic7, annee)]
message(sprintf("  années : %s", paste(range(freq_long$annee), collapse = "-")))

noter("gares du Grand Est -> fréquentation (UIC)", nrow(gares), sum(gares$uic7 %in% freq_long$uic7),
      "gares sans fréquentation : haltes non comptées ou UIC différent")
if (!is.na(c_cp)) {
  # sens inverse : gares de la fréquentation situées dans le Grand Est mais sans gare RFN
  uic_ge <- unique(norm_uic(freq[substr(gsub("\\D", "", get(c_cp)), 1, 2) %in% DEP_GRAND_EST, get(c_uic)]))
  noter("fréquentation (CP Grand Est) -> gares RFN", length(uic_ge), sum(uic_ge %in% gares$uic7))
}

# ---- 6. Covariables communales ----------------------------------------------

message("\n=== Covariables communales ===")

# Communes en géographie courante : centre, EPCI, population
ctr <- jsonlite::fromJSON(file.path(dir_raw, "geographie", "communes_grand_est_centres.json"))
xy_ctr <- do.call(rbind, ctr$centre$coordinates)
communes <- data.table(code = ctr$code, nom = ctr$nom, epci = ctr$codeEpci, dep = ctr$codeDepartement,
                       population = as.numeric(ctr$population), lon = xy_ctr[, 1], lat = xy_ctr[, 2])

# Table de passage : une colonne CODGEO_AAAA par année, en-tête précédé de lignes de titre
f_tp <- chercher_fichier("geographie", "passage.*\\.xlsx?$")
if (is.na(f_tp)) stop("table de passage introuvable dans ", file.path(dir_raw, "geographie"))
feuilles <- readxl::excel_sheets(f_tp)
feuille  <- c(feuilles[toupper(feuilles) == "COM"], feuilles)[1]
entete   <- readxl::read_excel(f_tp, sheet = feuille, col_names = FALSE, n_max = 30, col_types = "text",
                              .name_repair = "minimal")
skip     <- which(apply(entete, 1, \(r) any(grepl("^CODGEO_\\d{4}$", r))))[1] - 1
tp <- as.data.table(readxl::read_excel(f_tp, sheet = feuille, skip = skip, col_types = "text"))
annees_tp <- as.integer(sub("CODGEO_", "", grep("^CODGEO_\\d{4}$", names(tp), value = TRUE)))
GEO_CIBLE <- max(annees_tp)
message(sprintf("  table de passage : %d-%d, géographie cible %d", min(annees_tp), GEO_CIBLE, GEO_CIBLE))
noter(sprintf("communes API Géo -> table de passage %d", GEO_CIBLE), nrow(communes),
      sum(communes$code %in% tp[[paste0("CODGEO_", GEO_CIBLE)]]),
      "doit valoir 100 % : sinon l'API Géo et la table n'ont pas le même millésime")

passage <- function(annee) {
  a <- paste0("CODGEO_", annee)
  if (!a %in% names(tp)) stop("table de passage : pas de colonne ", a)
  m <- unique(tp[, .(code_source = get(a), code = get(paste0("CODGEO_", GEO_CIBLE)))])[!is.na(code_source)]
  # scission : une commune source -> plusieurs communes cibles, on garde la première
  n_sc <- m[, .N, by = code_source][N > 1, .N]
  if (n_sc) message(sprintf("  [info] %d communes de %d scindées depuis : rattachées à une seule commune", n_sc, annee))
  m[!duplicated(code_source)]
}

# Passe des communes de l'année source à la géographie cible en sommant des effectifs.
# Les médianes et les taux ne se somment pas : on somme des numérateurs et des
# dénominateurs, et on calcule les ratios après agrégation.
harmoniser <- function(dt, annee, vars, quoi) {
  m <- passage(annee)
  dt <- merge(dt, m, by.x = "codgeo", by.y = "code_source", all.x = TRUE)
  dt_ge <- dt[substr(codgeo, 1, 2) %in% DEP_GRAND_EST]
  noter(sprintf("%s (géo %d) -> géo %d", quoi, annee, GEO_CIBLE), nrow(dt_ge), sum(!is.na(dt_ge$code)))
  dt[!is.na(code), lapply(.SD, sum, na.rm = TRUE), by = code, .SDcols = vars]
}

# RP 2019, logement : motorisation des ménages
f_log <- chercher_fichier("covariables", "logement.*2019.*\\.csv$")
logt <- lire(f_log)
logt <- logt[, .(codgeo = get(col(logt, "^codgeo$", "RP logement")),
                 rp = nombre(get(col(logt, "^p19_rp$", "RP logement"))),
                 rp_voiture = nombre(get(col(logt, "^p19_rp_voit1p$", "RP logement"))))]
logt <- harmoniser(logt, GEO_RP2019, c("rp", "rp_voiture"), "RP 2019 logement")

# Filosofi 2021 : médiane et taux de pauvreté, pondérés par les personnes des
# ménages fiscaux. Valeurs absentes (secret statistique) : non pondérées, la
# couverture est gardée pour le contrôle.
f_fil <- chercher_fichier("covariables", "filosofi.*\\.csv$|^cc_filosofi.*\\.csv$")
fil <- lire(f_fil)
fil <- fil[, .(codgeo = get(col(fil, "^codgeo$", "Filosofi")),
               pers = nombre(get(col(fil, "^nbpersmenfisc\\d{2}$", "Filosofi"))),
               med  = nombre(get(col(fil, "^med\\d{2}$", "Filosofi"))),
               tp60 = nombre(get(col(fil, "^tp60\\d{2}$", "Filosofi"))))]
fil[is.na(pers), pers := 0]
fil[, `:=`(pers_med = fifelse(is.na(med), 0, pers), med_x_pers = fifelse(is.na(med), 0, med * pers),
           pers_tp60 = fifelse(is.na(tp60), 0, pers), pauvres = fifelse(is.na(tp60), 0, tp60 / 100 * pers))]
fil <- harmoniser(fil, GEO_FILOSOFI2021, c("pers", "pers_med", "med_x_pers", "pers_tp60", "pauvres"), "Filosofi 2021")

# MOBPRO : actifs par commune de résidence, mode de transport et lieu de travail.
# Les codes de TRANS sont lus dans le dictionnaire des modalités, pas codés en dur
# (ils ont changé entre recensements).
lire_mobpro <- function(annee, geo) {
  f  <- chercher_fichier("covariables", sprintf("^FD_MOBPRO_?%d.*\\.csv$", annee))
  fv <- chercher_fichier("covariables", sprintf("^varmod.*mobpro.*%d.*\\.csv$", annee))
  if (is.na(f) || is.na(fv)) { message(sprintf("  [absent] MOBPRO %d : fichier ou dictionnaire introuvable", annee)); return(NULL) }

  vm <- lire(fv)
  vm <- vm[get(col(vm, "^cod_var$", "dictionnaire MOBPRO")) == "TRANS"]
  lib <- norm_txt(vm[[col(vm, "^lib_mod$", "dictionnaire MOBPRO")]])
  cod <- vm[[col(vm, "^cod_mod$", "dictionnaire MOBPRO")]]
  code_voiture <- cod[grepl("voiture", lib)]
  code_tc      <- cod[grepl("transports? en commun", lib)]
  if (length(code_voiture) != 1 || length(code_tc) != 1) stop("MOBPRO ", annee, " : modalités de TRANS non reconnues")
  message(sprintf("  MOBPRO %d : TRANS voiture = %s, transports en commun = %s", annee, code_voiture, code_tc))

  mp <- fread(f, select = c("COMMUNE", "DCLT", "TRANS", "IPONDI"),
              colClasses = list(character = c("COMMUNE", "DCLT", "TRANS"), numeric = "IPONDI"))
  mp <- mp[substr(COMMUNE, 1, 2) %in% DEP_GRAND_EST]
  m <- passage(geo)
  mp[m, on = .(COMMUNE = code_source), code := i.code]
  mp[m, on = .(DCLT = code_source), code_travail := i.code]
  noter(sprintf("MOBPRO %d (géo %d) -> géo %d", annee, geo, GEO_CIBLE), uniqueN(mp$COMMUNE),
        uniqueN(mp[!is.na(code), COMMUNE]))
  ems <- communes[epci == EPCI_EMS, code]
  out <- mp[!is.na(code), .(actifs = sum(IPONDI),
                            actifs_voiture = sum(IPONDI[TRANS == code_voiture]),
                            actifs_tc      = sum(IPONDI[TRANS == code_tc]),
                            actifs_vers_ems = sum(IPONDI[code_travail %in% ems])), by = code]
  setnames(out, -1, paste0(names(out)[-1], "_", annee))
  out
}
mob19 <- lire_mobpro(2019, GEO_MOBPRO2019)
mob22 <- lire_mobpro(2022, GEO_MOBPRO2022)

cov_communes <- Reduce(\(x, y) if (is.null(y)) x else merge(x, y, by = "code", all.x = TRUE),
                       list(communes, logt, fil, mob19, mob22))

# ---- 7. Bassins de gare -----------------------------------------------------
# Chaque commune est rattachée à la gare la plus proche (distance entre centres,
# Lambert 93), si elle est à moins de RAYON_BASSIN_KM. L'Insee déconseille
# d'exploiter seules les communes de moins de 2 000 habitants : on agrège.

message("\n=== Bassins de gare ===")

pts_communes <- st_transform(st_as_sf(cov_communes, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 2154)
pts_gares    <- st_transform(st_as_sf(gares, coords = c("lon", "lat"), crs = 4326, remove = FALSE), 2154)
idx <- st_nearest_feature(pts_communes, pts_gares)
cov_communes[, `:=`(uic7_bassin = gares$uic7[idx],
                    dist_gare_km = as.numeric(st_distance(pts_communes, pts_gares[idx, ], by_element = TRUE)) / 1000)]
cov_communes[dist_gare_km > RAYON_BASSIN_KM, uic7_bassin := NA]
noter(sprintf("communes -> gare à moins de %d km", RAYON_BASSIN_KM), nrow(cov_communes), sum(!is.na(cov_communes$uic7_bassin)))

ratio <- function(num, den) fifelse(den > 0, num / den, NA_real_)
for (a in c(2019, 2022)) for (v in paste0(c("actifs_", "actifs_voiture_", "actifs_tc_", "actifs_vers_ems_"), a))
  if (!v %in% names(cov_communes)) cov_communes[, (v) := NA_real_]   # millésime MOBPRO absent

cov_bassins <- cov_communes[!is.na(uic7_bassin), .(
  n_communes = .N, population_bassin = sum(population, na.rm = TRUE),
  part_menages_sans_voiture = 1 - ratio(sum(rp_voiture, na.rm = TRUE), sum(rp, na.rm = TRUE)),
  revenu_median_pondere = ratio(sum(med_x_pers, na.rm = TRUE), sum(pers_med, na.rm = TRUE)),
  taux_pauvrete         = ratio(sum(pauvres, na.rm = TRUE), sum(pers_tp60, na.rm = TRUE)),
  couverture_filosofi   = ratio(sum(pers_med, na.rm = TRUE), sum(pers, na.rm = TRUE)),
  part_actifs_voiture_2019  = ratio(sum(actifs_voiture_2019), sum(actifs_2019)),
  part_actifs_tc_2019       = ratio(sum(actifs_tc_2019), sum(actifs_2019)),
  part_actifs_vers_ems_2019 = ratio(sum(actifs_vers_ems_2019), sum(actifs_2019)),
  part_actifs_voiture_2022  = ratio(sum(actifs_voiture_2022), sum(actifs_2022)),
  part_actifs_tc_2022       = ratio(sum(actifs_tc_2022), sum(actifs_2022)),
  part_actifs_vers_ems_2022 = ratio(sum(actifs_vers_ems_2022), sum(actifs_2022))),
  by = .(uic7 = uic7_bassin)]
noter("gares -> bassin non vide", nrow(gares), nrow(cov_bassins))

# ---- 8. Panel gare × année --------------------------------------------------

message("\n=== Panel gares ===")

panel <- merge(freq_long, gares, by = "uic7")           # gares du Grand Est seulement
panel <- merge(panel, cov_bassins, by = "uic7", all.x = TRUE)
panel[, annee_perturbee := mapply(\(a, p) nzchar(p) && grepl(a, p), annee, fifelse(is.na(annees_perturbees), "", annees_perturbees))]
panel[, post := annee >= 2023]                          # lancement le 11/12/2022 : 2022 = dernière année pré
setcolorder(panel, c("uic7", "nom_gare", "annee", "voyageurs", "statut", "segment_id", "exclusion", "post", "annee_perturbee"))
setorder(panel, nom_gare, annee)

# ---- 9. Comptages routiers -> gare la plus proche ---------------------------

message("\n=== Comptages routiers ===")

lire_comptage <- function(annee) {
  shp <- list.files(file.path(dir_raw, "routier", paste0("dreal_comptage_", annee)), "\\.shp$",
                    recursive = TRUE, full.names = TRUE)
  if (!length(shp)) { message(sprintf("  [absent] comptages %d", annee)); return(NULL) }
  rbindlist(lapply(shp, \(f) {
    x <- st_read(f, quiet = TRUE)
    if (is.na(st_crs(x))) { message("  [alerte] pas de projection, Lambert 93 supposé : ", basename(f)); st_crs(x) <- 2154 }
    x <- st_transform(x, 2154)
    if (!all(st_geometry_type(x) %in% c("POINT", "MULTIPOINT"))) x <- st_point_on_surface(st_make_valid(x))
    d <- st_drop_geometry(x)
    c_tmja <- col(d, "^tmja", paste("comptages", annee))
    c_pl   <- col(d, "pc_?pl|pct_?pl|ratio_?pl|^pl", paste("comptages", annee), obligatoire = FALSE)
    c_id   <- col(d, "^id|ident|station|^num", paste("comptages", annee), obligatoire = FALSE)
    c_rte  <- col(d, "route|^axe|^voie", paste("comptages", annee), obligatoire = FALSE)
    xy <- st_coordinates(st_geometry(x))[, 1:2, drop = FALSE]
    data.table(annee = annee, fichier = basename(f),
               id_source = if (is.na(c_id)) NA_character_ else as.character(d[[c_id]]),
               route = if (is.na(c_rte)) NA_character_ else as.character(d[[c_rte]]),
               tmja = nombre(d[[c_tmja]]),
               part_pl = if (is.na(c_pl)) NA_real_ else nombre(d[[c_pl]]),
               x_l93 = xy[, 1], y_l93 = xy[, 2])
  }), fill = TRUE)
}
comptages <- rbindlist(lapply(2017:2024, lire_comptage), fill = TRUE)

if (nrow(comptages)) {
  # identifiant stable entre années : la position arrondie à 100 m, les
  # identifiants des fichiers DREAL n'étant pas garantis d'une année à l'autre
  comptages[, id_station := sprintf("%.0f_%.0f", round(x_l93, -2), round(y_l93, -2))]
  pts <- st_as_sf(comptages, coords = c("x_l93", "y_l93"), crs = 2154, remove = FALSE)
  idx <- st_nearest_feature(pts, pts_gares)
  comptages[, `:=`(uic7_proche = gares$uic7[idx], gare_proche = gares$nom_gare[idx],
                   dist_gare_km = as.numeric(st_distance(pts, pts_gares[idx, ], by_element = TRUE)) / 1000,
                   segment_proche = gares$segment_id[idx], statut_proche = gares$statut[idx])]

  # Composition du panel : la couverture change en 2021 (fin des comptages CD67/68)
  nb_annees <- uniqueN(comptages$annee)
  cylindrees <- comptages[, .(n = uniqueN(annee)), by = id_station][n == nb_annees, id_station]
  comptages[, station_cylindree := id_station %in% cylindrees]
  message("  Stations par année (toutes / présentes toutes les années) :")
  print(comptages[, .(stations = uniqueN(id_station), cylindrees = uniqueN(id_station[station_cylindree])), by = annee][order(annee)],
        row.names = FALSE)
  noter("stations -> présentes toutes les années", uniqueN(comptages$id_station), length(cylindrees),
        "panel cylindré pour H2")
}

# ---- 10. Écriture -----------------------------------------------------------

message("\n=== Écriture ===")
ecrire(gares, "gares.csv")
ecrire(appartenances[, .(uic7, nom_gare, code_ligne, segment_id, ligne_service, statut)], "gares_segments.csv")
ecrire(cov_communes, "covariables_communes.csv")
ecrire(cov_bassins, "covariables_bassins.csv")
ecrire(panel, "panel_gares.csv")
if (nrow(comptages)) ecrire(comptages, "panel_stations_routieres.csv")
ecrire(rbindlist(rapport), "rapport_jointures.csv")
