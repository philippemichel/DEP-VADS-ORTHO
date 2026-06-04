# Générateur de données synthétiques pour DEP-VADS-ORTHO
# Ce script crée un jeu de données fictif pour tester le code d'analyse.
# NE PAS UTILISER pour l'analyse réelle — remplacer par les données vraies.

set.seed(2024)
n <- 60

# ---- Données démographiques ----
age <- round(rnorm(n, mean = 62, sd = 10))
age <- pmin(pmax(age, 30), 85)

sexe <- sample(c("Homme", "Femme"), n, replace = TRUE, prob = c(0.65, 0.35))

# ---- Caractéristiques tumorales ----
localisation <- sample(
  c("Cavité buccale", "Oropharynx", "Hypopharynx", "Larynx"),
  n, replace = TRUE, prob = c(0.25, 0.30, 0.15, 0.30)
)

stade <- sample(
  c("I-II", "III-IV"),
  n, replace = TRUE, prob = c(0.30, 0.70)
)

traitement <- sample(
  c("Chirurgie seule", "Radiothérapie ± chimio", "Chirurgie + radiothérapie ± chimio"),
  n, replace = TRUE, prob = c(0.20, 0.40, 0.40)
)

phase <- sample(
  c("Traitement actif", "Surveillance"),
  n, replace = TRUE, prob = c(0.55, 0.45)
)

delai_diag <- round(runif(n, min = 1, max = 36))  # mois depuis diagnostic

nb_seances <- round(runif(n, min = 1, max = 20))  # séances d'orthophonie

# ---- Distress Thermometer ----
# Score DT (0-10), distribution réaliste avec prévalence ~45% >= 4
DT_score <- sample(0:10, n, replace = TRUE,
                   prob = c(0.08, 0.07, 0.08, 0.07, 0.10, 0.12, 0.12, 0.10, 0.10, 0.08, 0.08))

DT_positif <- ifelse(DT_score >= 4, "Oui", "Non")

# Domaines de préoccupation DT (présence/absence)
DT_pratique  <- sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.35, 0.65))
DT_familial  <- sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.40, 0.60))
DT_emotionnel <- sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.55, 0.45))
DT_spirituel <- sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.15, 0.85))
DT_physique  <- sample(c("Oui", "Non"), n, replace = TRUE, prob = c(0.65, 0.35))

# ---- EORTC QLQ-HN35 (scores 0-100) ----
# Corrélation modérée attendue avec DT
make_hn35_score <- function(dt, base_mean, base_sd, r = 0.4) {
  noise <- rnorm(n, mean = 0, sd = base_sd * sqrt(1 - r^2))
  score <- base_mean + r * base_sd * scale(dt)[, 1] + noise
  pmin(pmax(round(score), 0), 100)
}

HN35_douleur    <- make_hn35_score(DT_score, 38, 25)
HN35_deglutition <- make_hn35_score(DT_score, 42, 28)
HN35_sensory    <- make_hn35_score(DT_score, 30, 22)
HN35_discours   <- make_hn35_score(DT_score, 35, 26)
HN35_manger_soc <- make_hn35_score(DT_score, 28, 24)
HN35_sexualite  <- make_hn35_score(DT_score, 20, 22)
HN35_dents      <- make_hn35_score(DT_score, 18, 20)
HN35_bouche_seche <- make_hn35_score(DT_score, 45, 28)
HN35_salive     <- make_hn35_score(DT_score, 40, 27)
HN35_toux       <- make_hn35_score(DT_score, 32, 24)
HN35_mal_aise   <- make_hn35_score(DT_score, 35, 25)
HN35_image      <- make_hn35_score(DT_score, 33, 26)
HN35_poids      <- make_hn35_score(DT_score, 28, 25)

# ---- Acceptabilité ----
accept_raw <- sample(1:4, n, replace = TRUE, prob = c(0.05, 0.10, 0.40, 0.45))
acceptabilite <- factor(
  accept_raw,
  levels = 1:4,
  labels = c("Pas du tout acceptable", "Peu acceptable",
             "Acceptable", "Tout à fait acceptable")
)

# ---- Orientation psychologique ----
# Probabilité plus élevée si DT >= 4
p_orient <- ifelse(DT_positif == "Oui", 0.60, 0.10)
oriente <- mapply(function(p) sample(c("Oui", "Non"), 1, prob = c(p, 1 - p)), p_orient)

# Parmi ceux orientés : ont-ils effectivement consulté ?
consulte <- ifelse(oriente == "Oui",
                   sample(c("Oui", "Non"), sum(oriente == "Oui"), replace = TRUE, prob = c(0.70, 0.30)),
                   NA_character_)
consulte_full <- rep(NA_character_, n)
consulte_full[oriente == "Oui"] <- consulte

# Délai en semaines (parmi ceux ayant consulté)
delai_consul <- ifelse(!is.na(consulte_full) & consulte_full == "Oui",
                       round(runif(n, 1, 8)),
                       NA_real_)

# ---- Assemblage du jeu de données ----
tt <- data.frame(
  age, sexe, localisation, stade, traitement, phase,
  delai_diag, nb_seances,
  DT_score, DT_positif,
  DT_pratique, DT_familial, DT_emotionnel, DT_spirituel, DT_physique,
  HN35_douleur, HN35_deglutition, HN35_sensory, HN35_discours,
  HN35_manger_soc, HN35_sexualite, HN35_dents, HN35_bouche_seche,
  HN35_salive, HN35_toux, HN35_mal_aise, HN35_image, HN35_poids,
  acceptabilite,
  oriente, consulte = consulte_full, delai_consul,
  stringsAsFactors = FALSE
)

# Introduire quelques données manquantes aléatoires (~5%)
set.seed(42)
for (col in c("HN35_sexualite", "HN35_dents", "delai_consul", "DT_pratique")) {
  idx <- sample(1:n, size = round(0.05 * n))
  tt[idx, col] <- NA
}

# Sauvegarde
save(tt, file = "data/fake_data.RData")
message("Jeu de données synthétique créé : ", nrow(tt), " patients, ", ncol(tt), " variables.")
message("Fichier sauvegardé : data/fake_data.RData")
