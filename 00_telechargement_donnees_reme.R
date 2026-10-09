# =============================================================================
# Évaluation du REME (Strasbourg) — Script 00 : téléchargement des données brutes
# -----------------------------------------------------------------------------
# Ce script ne fait QUE télécharger et archiver. Aucun nettoyage, aucune
# jointure, aucun filtrage : les fichiers sont conservés tels que publiés.
#
# Pour chaque fichier, le journal data/raw/journal_telechargements.csv garde :
# l'URL, la date de téléchargement, la taille, l'empreinte md5 et le statut.
# C'est important : SNCF révise l'historique de la fréquentation à chaque
# publication, il faut pouvoir dire quel millésime a été utilisé.
#
# Organisation (DIR_DONNEES/raw/) :
#   resultat/      fréquentation des gares, régularité TER
#   reseau/        gares, lignes RFN, horaires GTFS (actuel + historique)
#   routier/       comptages DREAL Grand Est, TMJA du réseau national
#   covariables/   MOBPRO 2019 et 2022, RP 2019 logement, Filosofi 2021
#   geographie/    communes du Grand Est (centres, contours), table de passage
#   foncier/       DVF géolocalisées (optionnel)
#   documentation/ notes et présentations qui documentent le traitement
#
# Dépendances : R de base uniquement (utils, tools).
# Relançable : un fichier déjà présent n'est pas retéléchargé, sauf
# FORCER_TELECHARGEMENT <- TRUE. Un téléchargement raté ne laisse pas de
# fichier partiel (écriture dans un fichier temporaire puis renommage).
# =============================================================================

# ---- 0. Paramètres ----------------------------------------------------------

DIR_DONNEES           <- "data"
FORCER_TELECHARGEMENT <- FALSE

# Blocs optionnels (volumineux ou secondaires)
INCLURE_DVF           <- FALSE   # volet foncier : ~ 10-30 Mo par année et par département
INCLURE_GEOJSON_RFN   <- TRUE    # tracés des lignes, pour les cartes
INCLURE_DOCUMENTATION <- TRUE    # PDF sources du statut REME des lignes

DVF_ANNEES       <- 2020:2025   # la diffusion ouverte couvre ~ 5 années glissantes
DVF_DEPARTEMENTS <- c("67")

# Horaires GTFS : dates cibles pour lesquelles on cherche, dans l'historique de
# transport.data.gouv.fr, la dernière version publiée AVANT la date. Un GTFS
# couvre ~ 150 jours à partir de sa publication : la version retenue contient
# donc les horaires en vigueur à la date cible.
GTFS_DATES_CIBLES <- as.Date(c(
  "2019-11-12",   # pré-période, avant COVID (probablement absente de l'historique)
  "2022-11-15",   # juste avant le REME (lancement le 11/12/2022)
  "2023-02-14",   # REME phase 1 (02/01/2023)
  "2023-10-10",   # REME phase 2 (21/08/2023)
  "2024-10-08",
  "2025-10-07"
))
GTFS_ID_JEU_PAN <- "1290"       # jeu « Réseau SNCF TGV, Intercités et TER »
GTFS_ECART_MAX_JOURS <- 120     # au-delà, la version ne couvre plus la date cible (~ 150 jours d'horaires)

# ---- 1. Catalogue des sources -----------------------------------------------
# role : à quoi sert la source dans l'évaluation (H1 = fréquentation,
# H2 = trafic routier, H3 = hétérogénéité, T = mesure du traitement).

src <- function(id, theme, fichier, url, role, description, decompresser = FALSE) {
  data.frame(id = id, theme = theme, fichier = fichier, url = url, role = role,
             description = description, decompresser = decompresser,
             stringsAsFactors = FALSE)
}

SNCF_EXPORT <- "https://ressources.data.sncf.com/api/explore/v2.1/catalog/datasets/%s/exports/%s"
atom_dreal  <- function(id) paste0("https://atom.geo-ide.developpement-durable.gouv.fr/atomArchive/GetResource?id=",
                                   id, "&dataType=dataset")

catalogue <- rbind(

  # --- Résultat : fréquentation (H1) ---
  src("sncf_frequentation", "resultat", "frequentation_gares.csv",
      sprintf(SNCF_EXPORT, "frequentation-gares", "csv?use_labels=true"),
      "H1", "Fréquentation annuelle des gares 2015-2024 (SNCF Gares & Connexions)"),
  src("sncf_regularite_ter", "resultat", "regularite_mensuelle_ter.csv",
      sprintf(SNCF_EXPORT, "regularite-mensuelle-ter", "csv?use_labels=true"),
      "contexte", "Régularité mensuelle des TER par région (démarrage du REME en 2023)"),

  # --- Réseau et traitement ---
  src("gc_gares_voyageurs", "reseau", "gares_de_voyageurs.csv",
      sprintf(SNCF_EXPORT, "gares-de-voyageurs", "csv?use_labels=true"),
      "T", "Référentiel des gares de voyageurs : codes UIC, coordonnées, commune"),
  src("rfn_liste_gares", "reseau", "rfn_liste_des_gares.csv",
      sprintf(SNCF_EXPORT, "liste-des-gares", "csv?use_labels=true"),
      "T", "SNCF Réseau : gares avec code ligne et point kilométrique"),
  src("rfn_lignes_statut", "reseau", "rfn_lignes_par_statut.csv",
      sprintf(SNCF_EXPORT, "lignes-par-statut", "csv?use_labels=true"),
      "T", "SNCF Réseau : lignes du réseau ferré national (code, libellé, statut)"),
  src("gtfs_sncf_actuel", "reseau", "gtfs_sncf_actuel.zip",
      "https://eu.ftp.opendatasoft.com/sncf/plandata/Export_OpenData_SNCF_GTFS_NewTripId.zip",
      "T", "Horaires théoriques SNCF (TER, IC, TGV) en vigueur, format GTFS", decompresser = TRUE),
  src("gtfs_historique_index", "reseau", "gtfs_historique_index.csv",
      paste0("https://transport.data.gouv.fr/datasets/", GTFS_ID_JEU_PAN, "/resources_history_csv"),
      "T", "Index des versions historisées du GTFS SNCF (transport.data.gouv.fr)"),

  # --- Routier : comptages (H2) ---
  src("dreal_comptage_2017", "routier", "dreal_comptage_2017.zip", atom_dreal("73e4de57-5f49-4005-baa6-6c037eb13376"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2017 (shapefile)", TRUE),
  src("dreal_comptage_2018", "routier", "dreal_comptage_2018.zip", atom_dreal("b51bb51d-6bf3-4226-9ffd-ec21cf0924d9"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2018 (shapefile)", TRUE),
  src("dreal_comptage_2019", "routier", "dreal_comptage_2019.zip", atom_dreal("561c3265-8250-4108-92e4-b2ee7e94e404"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2019 (shapefile)", TRUE),
  src("dreal_comptage_2020", "routier", "dreal_comptage_2020.zip", atom_dreal("c7adef2d-cf9c-4df2-b2bd-7bca8325bafd"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2020 (shapefile, crise sanitaire)", TRUE),
  src("dreal_comptage_2021", "routier", "dreal_comptage_2021.zip", atom_dreal("b834e24b-d8e0-4b07-a8bd-b4f2a9336fec"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2021 (shapefile)", TRUE),
  src("dreal_comptage_2022", "routier", "dreal_comptage_2022.zip", atom_dreal("fe8416fd-dc9a-4133-8e79-3299c8770e66"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2022 (shapefile)", TRUE),
  src("dreal_comptage_2023", "routier", "dreal_comptage_2023.zip", atom_dreal("c026d162-e116-4f01-afa4-0656d3c3a0cc"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2023 (shapefile)", TRUE),
  src("dreal_comptage_2024", "routier", "dreal_comptage_2024.zip", atom_dreal("0b61505a-2558-4377-9631-d81987f28e9c"),
      "H2", "Démarche CoMPTAGE, trafic routier Grand Est, 2024 (shapefile)", TRUE),
  src("tmja_rrn_2017", "routier", "tmja_rrn_2017.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20190206-140158/tmja-2017.csv",
      "H2", "TMJA du réseau routier national 2017 (non publié pour 2020-2021)"),
  src("tmja_rrn_2018", "routier", "tmja_rrn_2018.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20200601-111600/tmja2018.csv",
      "H2", "TMJA du réseau routier national 2018"),
  src("tmja_rrn_2019", "routier", "tmja_rrn_2019.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20211222-165040/tmja-2019.csv",
      "H2", "TMJA du réseau routier national 2019"),
  src("tmja_rrn_2022", "routier", "tmja_rrn_2022.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20250818-095350/tmja-rrnc-2022.csv",
      "H2", "TMJA du réseau routier national 2022"),
  src("tmja_rrn_2023", "routier", "tmja_rrn_2023.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20250818-095712/tmja-rrnc-2023.csv",
      "H2", "TMJA du réseau routier national 2023"),
  src("tmja_rrn_2024", "routier", "tmja_rrn_2024.csv",
      "https://static.data.gouv.fr/resources/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national/20250818-100154/tmja-rrnc-2024.csv",
      "H2", "TMJA du réseau routier national 2024"),

  # --- Covariables de bassin (H3, conditionnement) : millésimes pré-traitement ---
  src("mobpro_2019", "covariables", "RP2019_mobpro_csv.zip",
      "https://www.insee.fr/fr/statistiques/fichier/6456056/RP2019_mobpro_csv.zip",
      "H3", "RP 2019, mobilités domicile-travail (collectes 2017-2021) — spécification principale", TRUE),
  src("mobpro_2019_doc", "covariables", "contenu_RP2019_mobpro.pdf",
      "https://www.insee.fr/fr/statistiques/fichier/6456056/contenu_RP2019_mobpro.pdf",
      "H3", "Dictionnaire des variables MOBPRO 2019"),
  src("mobpro_2022", "covariables", "RP2022_mobpro.zip",
      "https://www.insee.fr/fr/statistiques/fichier/8589904/RP2022_mobpro.zip",
      "H3", "RP 2022, mobilités domicile-travail (collectes 2020-2024) — robustesse", TRUE),
  src("mobpro_2022_varmod", "covariables", "varmod_mobpro_2022.csv",
      "https://www.insee.fr/fr/statistiques/fichier/8589904/varmod_mobpro_2022.csv",
      "H3", "Modalités des variables MOBPRO 2022"),
  src("rp2019_logement", "covariables", "base-ccc-logement-2019.zip",
      "https://www.insee.fr/fr/statistiques/fichier/6454155/base-ccc-logement-2019.zip",
      "H3", "RP 2019, base communale logement (motorisation des ménages : P19_RP_VOIT1P…)", TRUE),
  src("filosofi_2021", "covariables", "filosofi_2021_communes_csv.zip",
      "https://www.insee.fr/fr/statistiques/fichier/7756855/indic-struct-distrib-revenu-2021-COMMUNES_csv.zip",
      "H3", "Filosofi 2021, revenus et niveaux de vie par commune (géographie 2022)", TRUE),

  # --- Géographie ---
  src("communes_ge_centres", "geographie", "communes_grand_est_centres.json",
      "https://geo.api.gouv.fr/communes?codeRegion=44&fields=code,nom,centre,codeEpci,codeDepartement,population&format=json&geometry=centre",
      "géo", "Communes du Grand Est : centre, EPCI, département, population (API Géo)"),
  src("communes_ge_contours", "geographie", "communes_grand_est_contours.geojson",
      "https://geo.api.gouv.fr/communes?codeRegion=44&fields=code,nom,codeEpci,codeDepartement&format=geojson&geometry=contour",
      "géo", "Communes du Grand Est : contours (API Géo)"),
  src("table_passage_communes", "geographie", "table_passage_annuelle_2026.zip",
      "https://www.insee.fr/fr/statistiques/fichier/7671867/table_passage_annuelle_2026.zip",
      "géo", "Table de passage des communes 2003-2026 (harmoniser RP 2019, Filosofi 2021, MOBPRO 2022)", TRUE)
)

if (INCLURE_GEOJSON_RFN)
  catalogue <- rbind(catalogue,
    src("rfn_lignes_geojson", "reseau", "rfn_lignes_par_statut.geojson",
        sprintf(SNCF_EXPORT, "lignes-par-statut", "geojson"),
        "carte", "SNCF Réseau : tracés des lignes (GeoJSON)"),
    src("rfn_gares_geojson", "reseau", "rfn_liste_des_gares.geojson",
        sprintf(SNCF_EXPORT, "liste-des-gares", "geojson"),
        "carte", "SNCF Réseau : gares géolocalisées (GeoJSON)"))

if (INCLURE_DVF)
  catalogue <- rbind(catalogue, do.call(rbind, unlist(lapply(DVF_ANNEES, \(a) lapply(DVF_DEPARTEMENTS, \(d)
    src(paste0("dvf_", a, "_", d), "foncier", sprintf("dvf_%s_%s.csv.gz", a, d),
        sprintf("https://files.data.gouv.fr/geo-dvf/latest/csv/%s/departements/%s.csv.gz", a, d),
        "foncier", sprintf("DVF géolocalisées, %s, département %s", a, d)))), recursive = FALSE)))

if (INCLURE_DOCUMENTATION)
  catalogue <- rbind(catalogue,
    src("doc_adeus_340", "documentation", "adeus_note_340_reme_2024.pdf",
        "https://www.adeus.org/wp-content/uploads/Note-340_REMe.pdf",
        "doc", "ADEUS, note n° 340, le REME (avril 2024) : offre par gare 2022-2024"),
    src("doc_corest_centre_alsace_2022", "documentation", "region_corest_centre_alsace_sa2023.pdf",
        "https://www.grandest.fr/wp-content/uploads/2022/10/corest-centre-alsace-vf.pdf",
        "doc", "Région, CoReST Centre Alsace : offre SA 2023 (Piémont, Bruche, Sélestat, Molsheim)"),
    src("doc_corest_smspp_2022", "documentation", "region_corest_smspp_sa2023.pdf",
        "https://www.grandest.fr/wp-content/uploads/2022/10/presentation-corest-smspp.pdf",
        "doc", "Région, CoReST Saverne–Sarrebourg : offre SA 2023 (Strasbourg–Saverne–Sarrebourg, Metz–Sarrebourg)"),
    src("doc_corest_smspp_cr_2022", "documentation", "region_corest_smspp_cr_20221013.pdf",
        "https://www.grandest.fr/wp-content/uploads/2023/03/20221013-cr-corest-smspp.pdf",
        "doc", "Région, compte rendu CoReST SMSPP du 13/10/2022 (travaux Mommenheim–Sarreguemines 2023)"),
    src("doc_corest_centre_alsace_cr_2023", "documentation", "region_corest_centre_alsace_cr_2023.pdf",
        "https://www.grandest.fr/wp-content/uploads/2024/06/cr-corest-centre-alsace-2023-v2.pdf",
        "doc", "Région, compte rendu CoReST Centre Alsace 2023"),
    src("doc_ems_reme_brochure", "documentation", "ems_region_brochure_reme_2022.pdf",
        "https://www.strasbourg.eu/documents/976405/410304191/0/9a88cf86-da67-dd56-e5e0-0fe19d486208",
        "doc", "Eurométropole et Région, brochure de lancement du REME (objectifs)"),
    src("doc_ems_conseil_20220204", "documentation", "ems_conseil_20220204.pdf",
        "https://www.oberhausbergen.com/wp-content/uploads/2022/11/22_02_04_Conseil-de-lEMS.pdf",
        "doc", "Conseil de l'Eurométropole du 04/02/2022 (ligne de Lauterbourg exclue du renfort)"),
    src("doc_dreal_comptage_methodo", "documentation", "dreal_fiche_methodologique_comptage.pdf",
        "https://www.grand-est.developpement-durable.gouv.fr/IMG/pdf/fiche_methodologique_comptage-3.pdf",
        "doc", "DREAL, fiche méthodologique des données de comptage (variables TMJA, NAPL…)"),
    src("doc_dreal_diffusion_2022", "documentation", "dreal_note_diffusion_2022.pdf",
        "https://www.grand-est.developpement-durable.gouv.fr/IMG/pdf/notecomptage_diffdonnees2022.pdf",
        "doc", "DREAL, note de diffusion des comptages 2022 (sources disponibles)"),
    src("doc_dreal_diffusion_2023", "documentation", "dreal_note_diffusion_2023.pdf",
        "https://www.grand-est.developpement-durable.gouv.fr/IMG/pdf/notecomptage_diffdonnees2023.pdf",
        "doc", "DREAL, note de diffusion des comptages 2023"),
    src("doc_dreal_diffusion_2024", "documentation", "dreal_note_diffusion_2024.pdf",
        "https://www.grand-est.developpement-durable.gouv.fr/IMG/pdf/notecomptage_diffdonnees2024.pdf",
        "doc", "DREAL, note de diffusion des comptages 2024"))

stopifnot(!anyDuplicated(catalogue$id))

# ---- 2. Fonctions -----------------------------------------------------------

dir_raw <- file.path(DIR_DONNEES, "raw")
fichier_journal <- file.path(dir_raw, "journal_telechargements.csv")
dir.create(dir_raw, recursive = TRUE, showWarnings = FALSE)
options(timeout = max(1200, getOption("timeout")))

est_zip <- function(chemin) {
  con <- file(chemin, "rb"); on.exit(close(con))
  identical(readBin(con, "raw", 2), as.raw(c(0x50, 0x4b)))   # signature « PK »
}

journaliser <- function(ligne) {
  journal <- if (file.exists(fichier_journal))
    utils::read.csv(fichier_journal, colClasses = "character", fileEncoding = "UTF-8") else NULL
  if (!is.null(journal)) journal <- journal[journal$id != ligne$id, , drop = FALSE]
  utils::write.csv(rbind(journal, ligne), fichier_journal, row.names = FALSE, fileEncoding = "UTF-8")
}

telecharger_un <- function(id, theme, fichier, url, decompresser = FALSE, essais = 3) {
  dossier <- file.path(dir_raw, theme)
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  chemin <- file.path(dossier, fichier)

  if (file.exists(chemin) && !FORCER_TELECHARGEMENT) {
    if (decompresser && !est_zip(chemin)) {
      message(sprintf("  [cache invalide] %-20s pas une archive zip : nouveau téléchargement", id))
    } else {
      message(sprintf("  [cache]  %-28s %s", id, fichier))
      return(invisible("cache"))
    }
  }

  tmp <- paste0(chemin, ".part")
  statut <- "echec"; msg <- ""
  for (k in seq_len(essais)) {
    res <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      "ok"
    }, error = \(e) conditionMessage(e), warning = \(w) conditionMessage(w))
    if (identical(res, "ok") && file.exists(tmp) && file.size(tmp) > 0) { statut <- "ok"; break }
    msg <- res
    if (k < essais) Sys.sleep(5 * k)    # le portail SNCF limite parfois le débit (HTTP 429)
  }

  if (statut == "ok") {
    file.rename(tmp, chemin)
    if (decompresser) {
      if (est_zip(chemin)) {
        utils::unzip(chemin, exdir = file.path(dossier, sub("\\.zip$", "", fichier)))
      } else {
        statut <- "ok_non_zip"
        msg <- "fichier reçu mais pas une archive zip : l'ouvrir pour vérifier (page d'erreur ?)"
      }
    }
    message(sprintf("  [%s] %-28s %s (%.1f Mo)", if (statut == "ok") "ok    " else "alerte",
                    id, fichier, file.size(chemin) / 1e6))
  } else {
    if (file.exists(tmp)) file.remove(tmp)
    message(sprintf("  [ÉCHEC] %-28s %s", id, msg))
  }

  journaliser(data.frame(
    id = id, theme = theme, fichier = file.path(theme, fichier), url = url,
    date_telechargement = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    octets = if (file.exists(chemin)) as.character(file.size(chemin)) else "",
    md5 = if (file.exists(chemin)) unname(tools::md5sum(chemin)) else "",
    statut = statut, message = msg, stringsAsFactors = FALSE))
  invisible(statut)
}

# ---- 3. Téléchargement du catalogue -----------------------------------------

message("\n=== Téléchargement de ", nrow(catalogue), " sources ===")
statuts <- vapply(seq_len(nrow(catalogue)), \(i) with(catalogue[i, ],
  telecharger_un(id, theme, fichier, url, decompresser)), character(1))
catalogue$statut <- statuts

# ---- 4. GTFS historiques ----------------------------------------------------
# Lit l'index des versions archivées et, pour chaque date cible, télécharge la
# dernière version GTFS publiée avant cette date. Les versions antérieures au
# début de l'archivage ne peuvent pas être récupérées ici.

message("\n=== Horaires GTFS historiques ===")
f_index <- file.path(dir_raw, "reseau", "gtfs_historique_index.csv")
gtfs_retenus <- NULL

if (file.exists(f_index)) {
  index <- utils::read.csv(f_index, colClasses = "character", fileEncoding = "UTF-8")
  index <- index[grepl('"format":\\s*"GTFS"', index$payload), , drop = FALSE]
  index$date_publication <- as.Date(substr(index$inserted_at, 1, 10))

  if (nrow(index) == 0) {
    message("  aucune version GTFS dans l'index")
  } else {
    message(sprintf("  %d versions GTFS archivées, du %s au %s", nrow(index),
                    min(index$date_publication), max(index$date_publication)))
    for (d in as.character(GTFS_DATES_CIBLES)) {
      avant <- index[index$date_publication <= as.Date(d), , drop = FALSE]
      if (nrow(avant) == 0) {
        message(sprintf("  [indisponible] %s : aucune version archivée avant cette date", d))
        next
      }
      v <- avant[which.max(avant$date_publication), ]
      ecart <- as.integer(as.Date(d) - v$date_publication)
      if (ecart > GTFS_ECART_MAX_JOURS) {
        message(sprintf("  [indisponible] %s : dernière version publiée %d jours avant (ne couvre pas la date)", d, ecart))
        next
      }
      telecharger_un(paste0("gtfs_", gsub("-", "", d)), "reseau",
                     sprintf("gtfs_sncf_cible_%s_publie_%s.zip", d, v$date_publication),
                     v$permanent_url, decompresser = TRUE)
      gtfs_retenus <- rbind(gtfs_retenus, data.frame(date_cible = d, date_publication = as.character(v$date_publication),
                                                     url = v$permanent_url))
    }
  }
} else {
  message("  index absent : téléchargement de l'index échoué, voir le journal")
}

# ---- 5. Bilan ---------------------------------------------------------------

utils::write.csv(catalogue, file.path(dir_raw, "catalogue_sources.csv"), row.names = FALSE, fileEncoding = "UTF-8")
if (!is.null(gtfs_retenus))
  utils::write.csv(gtfs_retenus, file.path(dir_raw, "reseau", "gtfs_versions_retenues.csv"), row.names = FALSE)

message("\n=== Bilan ===")
print(table(catalogue$theme, catalogue$statut))
echecs <- catalogue[!catalogue$statut %in% c("ok", "cache"), c("id", "statut", "url")]
if (nrow(echecs) > 0) {
  message("\nÀ reprendre (relancer le script, ou télécharger à la main) :")
  print(echecs, row.names = FALSE)
}

message("\nÀ récupérer hors de ce script :")
message("  - GTFS des services 2019-2022 s'ils manquent dans l'historique : demande à la Région ou à Fluo")
message("    (service annuel 2022 et 2023), ou API de la Mobility Database (compte gratuit).")
message("  - Offre de la ligne Haguenau–Niederbronn 2022-2024 : aucune source publique trouvée.")
message("  - Calendrier des travaux par ligne 2019-2024 : SNCF Réseau / Région.")
message("\nJournal : ", normalizePath(fichier_journal))
