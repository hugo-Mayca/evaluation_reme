# Évaluation quantitative du REME (Strasbourg) — contexte du projet

Document de passation. Il résume les décisions prises lors du cadrage (octobre 2026).
Toute affirmation factuelle est accompagnée de sa source ; ce qui n'a pas pu être
vérifié est signalé comme tel.

## Règles de travail

- **Hugo code le projet lui-même, en R.** Le rôle de Claude : relire, conseiller,
  challenger, expliquer les méthodes, aider à récupérer des données. Ne pas écrire
  le code d'analyse à sa place sauf demande explicite.
- Challenger les hypothèses avec des arguments factuels ou théoriques, ne pas
  valider par défaut.
- Explications mathématiques : format « cours » (définitions et équations d'abord,
  variables expliquées, comment lire l'équation, puis intuition et analogies).
- Pas de données nominatives dans les sorties.

## Objet et enjeu

- **Politique évaluée** : le REME (Réseau express métropolitain européen), lancé le
  11/12/2022 par la Région Grand Est et l'Eurométropole de Strasbourg (EMS), sur
  la base d'un accord signé en mai 2021. Il devient le SERM à partir de 2026
  (cofinancement des surcoûts d'exploitation 2026-2028, ~9,5 M€/an pour l'EMS).
- **Phases** : 11/12/2022 (813 trains/semaine en plus), 02/01/2023 (+67),
  21/08/2023 (+192) — Banque des Territoires.
- **Enjeu pour Hugo** : étude vitrine pour sa prospection freelance (Eurométropole,
  Région, ADEUS). Positionnement vis-à-vis de l'ADEUS : complémentaire, pas
  correctif. La note ADEUS n° 340 (avril 2024) mesure l'**offre** (arrêts par gare) ;
  l'étude mesure l'**effet sur les usages**. Formule : « l'offre a été mesurée,
  l'effet sur les usages reste à mesurer ».

## Objectifs officiels (base des hypothèses)

Brochure de lancement EMS–Région (2022) : diminuer le recours à la voiture
(notamment sur les trajets quotidiens longs), baisser les émissions de GES, lutter
contre la congestion, améliorer les possibilités de déplacement (ménages exposés au
coût du carburant). **Seule l'offre est chiffrée** (jusqu'à 120 TER/jour en plus,
+43 % d'offre, cadence 30 min de 5 h à 23 h). Aucune cible de fréquentation, de
part modale ou de trajets évités ; aucun dispositif d'évaluation annoncé.

## Problématique et hypothèses

**Le REME a-t-il produit le report modal visé, ou seulement une hausse de
fréquentation ?** Chaîne causale : offre → usage du train → baisse de la voiture
→ congestion / émissions. L'étude teste les maillons dans l'ordre.

| | Hypothèse | Indicateur | Données | Statut |
|---|---|---|---|---|
| H1 | Le renfort d'offre augmente la fréquentation, d'autant plus que le renfort est fort | voyageurs/gare/an ; élasticité à l'offre | SNCF fréquentation + GTFS | cœur |
| H2 | Le trafic auto baisse sur les axes parallèles aux lignes renforcées | TMJA par station | DREAL CoMPTAGE 2017-2024 | cœur, bruité |
| H3 | Effet plus fort dans les bassins modestes / dépendants de la voiture | hétérogénéité de H1 | Filosofi 2021, RP 2019 | complément |
| H4 | Moins d'émissions | t CO₂ évitées | H2 × facteurs ADEME | calcul conditionnel à H2 |
| H5 | Moins de congestion | temps de parcours | non disponible en open data | hors champ (limites) |

Points d'attention :
- H1 seul ne distingue pas report depuis la voiture, depuis les cars, ou mobilité
  induite → H2 est indispensable. Un H2 nul est un résultat utile.
- H2 : le GCO (A355) ouvre mi-décembre 2021 avec interdiction du transit PL sur
  l'A35/M35 → exclure les stations touchées ou traiter ce choc à part. Faire un
  calcul de puissance (effet minimal détectable).
- H3 : le pic des carburants de 2022 tombe dans la dernière année pré → tester
  la robustesse avec 2019 comme année de référence.
- Le RP ne sert pas de résultat (collectes glissantes sur 5 ans, REME dilué).

## Design

- **Unité** : gare × année. Résultat principal : log(voyageurs).
- **Traitement** : défini **par ligne**, pas par département ni par distance.
  À terme, intensité continue = variation des dessertes par gare (GTFS).
- **Spécification principale** : lignes REME vs **lignes non renforcées du
  Bas-Rhin** (témoins proches : même bassin d'emploi, mêmes chocs régionaux).
- **Robustesses** : (1) témoins éloignés du Grand Est hors lignes modifiées en
  2023 ; (2) exclusion des témoins proches d'une gare REME (débordements :
  rabattement en voiture vers une gare mieux desservie) ; (3) intensité continue.
- **Estimation** : event study (effet dynamique, démarrage chaotique en 2023) ;
  2022 = dernière année pré (lancement le 11/12). COVID 2020-2021 à traiter à part.
  Covariables pré-traitement uniquement (`did::att_gt`, `xformla`).
- **Inférence** : peu de lignes témoins (4-5 clusters) → erreurs-types classiques
  non fiables → wild cluster bootstrap ou inférence par permutation.

## Statut des lignes (détail et sources : `table_traitement_lignes.csv`)

| Statut | Ligne | Offre en semaine, SA 2023 |
|---|---|---|
| traitée | Strasbourg – Saverne – Sarrebourg (inclut Lutzelbourg, Sarrebourg, Moselle) | +83 % (CoReST SMSPP) |
| traitée | Strasbourg – Molsheim | +75 % (CoReST Centre Alsace) |
| traitée | Strasbourg – Sélestat (omnibus) | +46 % (CoReST Centre Alsace) |
| traitée | Strasbourg – Haguenau | ~+41 % (Bischwiller 82 → 116 arrêts/j, ADEUS) |
| traitée | Piémont (Molsheim – Barr – Sélestat) | semaine non publiée ; samedi +96 % |
| traitée faiblement | Vallée de la Bruche | +22 % |
| témoin proche | Strasbourg – Lauterbourg | pas de hausse ; travaux d'urgence 2020-2022 ; fret DB +40 trains/j en août 2024 |
| témoin proche | Mommenheim – Sarreguemines | marginal ; **fermée du 30/06 au 03/09/2023** → neutraliser 2023 |
| réduite | Haguenau – Wissembourg | trains supprimés après le lancement (Rue89, 07/2024) → à part |
| à vérifier | Haguenau – Niederbronn | aucune source trouvée — meilleur candidat témoin si stable |
| à vérifier | Sélestat – Colmar – Mulhouse | TER 200 « non impacté », omnibus non documentés |
| exclue | Strasbourg – Kehl – Offenbourg | transfrontalier |
| exclue des témoins | Metz – Sarrebourg ; Sarrebourg – Nancy | renforts hors REME en 2023 |

Exclusions de gares : Strasbourg-Ville (destination, mêle TGV/international/TER),
pôles en bout de segment desservis par d'autres lignes (Lauterbourg,
Sarreguemines, Saint-Dié), bassins très frontaliers (Luxembourg, Bâle).

## Données (téléchargement : `00_telechargement_donnees_reme.R`)

Le script ne fait que télécharger dans `data/raw/` (journal avec date, taille, md5).
Adresses relevées sur les pages officielles, non testées en conditions réelles.

- **Fréquentation en gares** (SNCF) : 2015-2024, billetterie SNCF, hors billets
  vendus par des tiers. **Historique révisé à chaque publication** : figer un
  millésime et le dater.
- **Réseau** : gares de voyageurs (UIC, coordonnées) ; SNCF Réseau « Liste des
  gares » (code ligne + PK) et « Lignes par statut ». Codes ligne écrits
  différemment selon les fichiers (zéros de tête) → normaliser avant jointure.
- **GTFS** : actuel + versions historisées de transport.data.gouv.fr (jeu 1290).
  Profondeur de l'archive inconnue ; si la version de novembre 2022 manque →
  demande à la Région / Fluo.
- **Routier** : DREAL CoMPTAGE 2017-2024 (shapefiles, TMJA/NAPL/NOPL ; CD67/68
  seulement 2017-2020, CeA permanentes seules en 2021-2022) ; TMJA du réseau
  national (2020-2021 non publiés).
- **Covariables** : MOBPRO **2019** en principal (collectes 2017-2021, entièrement
  pré-REME) ; MOBPRO 2022 en robustesse seulement (collectes 2020-2024, en partie
  post) ; RP 2019 logement (motorisation) ; Filosofi 2021 (communes). L'Insee
  déconseille l'exploitation complémentaire sous 2 000 habitants → agréger sur un
  bassin de gare.
- **Géographie** : table de passage des communes 2003-2026 (indispensable pour
  croiser RP 2019, Filosofi 2021 géo 2022, MOBPRO 2022).

## Chocs concomitants à documenter

- Phases REME : 11/12/2022, 02/01/2023, 21/08/2023.
- Ticket CTS valable dans le TER intra-EMS : septembre 2024 (gonfle 2024 dans
  les gares de l'EMS).
- COVID 2020-2021 ; ticket à 9 € (juin-août 2022) ; Deutschlandticket (mai 2023,
  ne permet pas d'aller jusqu'à Strasbourg en train).
- GCO + interdiction du transit PL sur l'A35 : décembre 2021.
- Grèves des retraites début 2023 ; travaux par ligne (calendrier à obtenir).

## Points ouverts

1. GTFS 2019-2022 si absents de l'archive.
2. Offre Haguenau – Niederbronn 2022-2024.
3. Calendrier des travaux par ligne 2019-2024 (Région / SNCF Réseau).
4. Variation de l'offre en semaine sur le Piémont.
5. Dossier de labellisation SERM : contient peut-être des objectifs chiffrés.

## Sources principales

- ADEUS, note n° 340 (avril 2024) : https://www.adeus.org/wp-content/uploads/Note-340_REMe.pdf
- Région, CoReST Centre Alsace, SA 2023 : https://www.grandest.fr/wp-content/uploads/2022/10/corest-centre-alsace-vf.pdf
- Région, CoReST SMSPP, SA 2023 : https://www.grandest.fr/wp-content/uploads/2022/10/presentation-corest-smspp.pdf
- Région, CR CoReST SMSPP 13/10/2022 : https://www.grandest.fr/wp-content/uploads/2023/03/20221013-cr-corest-smspp.pdf
- Brochure de lancement REME : https://www.strasbourg.eu/documents/976405/410304191/0/9a88cf86-da67-dd56-e5e0-0fe19d486208
- EMS, « Le REME devient le SERM » : https://www.strasbourg.eu/-/bloc-notes-le-reme-devient-le-serm-une-nouvelle-etape-pour-les-mobilites-du-bassin-de-vie-strasbourgeois
- Conseil de l'EMS 04/02/2022 : https://www.oberhausbergen.com/wp-content/uploads/2022/11/22_02_04_Conseil-de-lEMS.pdf
- Rue89 Strasbourg, 18/07/2024 : https://www.rue89strasbourg.com/reseau-express-metropolitain-mieux-en-mieux-pas-partout-308443
- Banque des Territoires, REME : https://www.banquedesterritoires.fr/le-reseau-express-metropolitain-europeen-de-strasbourg-un-nouveau-systeme-de-mobilite-multimodal
- Géoconfluences, R. Woessner (déc. 2022), REME et périurbanisation : https://geoconfluences.ens-lyon.fr/actualites/veille/breves/reme-strasbourg
- Fréquentation en gares : https://www.data.gouv.fr/datasets/frequentation-en-gares
