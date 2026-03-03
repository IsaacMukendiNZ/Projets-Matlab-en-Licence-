# ==============================================================================
# SCRIPT FINAL : ANALYSES HDF (AFFICHAGE NETTOYÉ ET GRAPHIQUES OPTIMISÉS)
# ==============================================================================
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra") 

library(readxl)
library(writexl)
library(ggplot2)
library(gridExtra)

# --- 1. IMPORTATION SANS MESSAGES PARASITES ---
cat("\n--- ÉTAPE 1 : IMPORTATION DES DONNÉES ---")
cat("\n[FICHIER 1] Veuillez choisir : EFFECTIFS DES NAISSANCES\n")
f_eff  <- file.choose()
cat("\n[FICHIER 2] Veuillez choisir : TAUX DE MORTINATALITÉ\n")
f_mort <- file.choose()
cat("\n[FICHIER 3] Veuillez choisir : TAUX DE PRÉMATURITÉ\n")
f_prem <- file.choose()

# .name_repair = "minimal" supprime le listing des colonnes ...1, ...2 dans la console
df_eff  <- read_excel(f_eff,  sheet = 2, col_names = FALSE, .name_repair = "minimal")
df_mort <- read_excel(f_mort, sheet = 2, col_names = FALSE, .name_repair = "minimal")
df_prem <- read_excel(f_prem, sheet = 2, col_names = FALSE, .name_repair = "minimal")

# --- 2. EXTRACTION DES LIGNES HDF ---
ligne_hdf_eff  <- which(df_eff[[2]] == "Hauts-de-France")[1] 
ligne_hdf_mort <- which(df_mort[[1]] == "Hauts-de-France")[1]
ligne_hdf_prem <- which(df_prem[[2]] == "Hauts-de-France")[1]
if(is.na(ligne_hdf_prem)) ligne_hdf_prem <- 16 

# --- 3. BOUCLE DE CALCUL 2012-2023 ---
res_final <- data.frame()
for (i in 0:11) {
  annee <- 2012 + i
  
  idx_tx_m    <- 2 + (i * 4)   
  idx_ef_m    <- idx_tx_m + 3
  idx_nat_tot <- 3 + (i * 2)   
  
  # Logique Prématurité : Col 17 (Taux) & 18 (Effectif), Pas 16
  col_taux_prem <- 17 + (i * 16)
  col_eff_prem  <- 18 + (i * 16)
  
  if (idx_tx_m <= ncol(df_mort) & idx_nat_tot <= ncol(df_eff)) {
    val_tx_M  <- as.numeric(df_mort[ligne_hdf_mort, idx_tx_m])
    val_ef_M  <- as.numeric(df_mort[ligne_hdf_mort, idx_ef_m])
    val_nat_Y <- as.numeric(df_eff[ligne_hdf_eff, idx_nat_tot])
    
    if (col_eff_prem <= ncol(df_prem)) {
      t_prem <- as.numeric(df_prem[ligne_hdf_prem, col_taux_prem])
      e_prem <- as.numeric(df_prem[ligne_hdf_prem, col_eff_prem])
      val_prem_calc <- round((t_prem * e_prem) / 100, 1)
    } else {
      val_prem_calc <- NA
    }
    
    res_final <- rbind(res_final, data.frame(
      Annee = annee,
      Naissances = val_nat_Y,
      Mortinatalite = round((val_ef_M * val_tx_M) / 100, 1),
      Prematures = ifelse(is.na(val_prem_calc), 0, val_prem_calc)
    ))
  }
}

# --- 4. BASE JOURNALIÈRE (2019-2023) ---
base_journaliere <- data.frame()
annees_simu <- intersect(2019:2023, res_final$Annee)

for (an in annees_simu) {
  dates <- seq(as.Date(paste0(an, "-01-01")), as.Date(paste0(an, "-12-31")), by="day")
  n_j   <- length(dates)
  data_an <- res_final[res_final$Annee == an, ]
  
  distrib <- function(total, n) {
    v <- rep(0, n)
    if(!is.na(total) && total > 0) {
      indices <- sample(1:n, round(total), replace = TRUE)
      tab <- table(indices)
      v[as.numeric(names(tab))] <- as.vector(tab)
    }
    return(v)
  }
  
  tm <- round(runif(n_j, 12, 15), 2)
  vague <- rep(0, n_j)
  idx_ete <- which(as.numeric(format(dates, "%m")) %in% 6:8)
  
  for(v in 1:sample(1:3, 1)) {
    duree <- sample(4:11, 1)
    debut <- sample(idx_ete[1]:(idx_ete[length(idx_ete)] - duree), 1)
    vague[debut:(debut + duree - 1)] <- 1
    tm[debut:(debut + duree - 1)] <- tm[debut:(debut + duree - 1)] + runif(duree, 8, 14)
  }
  
  base_journaliere <- rbind(base_journaliere, data.frame(
    date = dates, annee = an,
    naissance = distrib(data_an$Naissances, n_j),
    deces = distrib(data_an$Mortinatalite, n_j),
    naissance_prematuree = distrib(data_an$Prematures, n_j),
    TM = tm, vague_chaleur = vague
  ))
}

# --- 5. GRAPHIQUES (AMÉLIORÉS POUR LA VISIBILITÉ 2019) ---
df_viz <- res_final %>% filter(Annee >= 2019)

# Fonction pour ajouter du style et de l'espace pour les étiquettes
style_graph <- function(p, titre, var_y) {
  p + geom_line(color="black", size=0.8, alpha=0.5) + 
    geom_point(size=3, color="red") +
    geom_text(aes(label=var_y), vjust=-1.2, fontface="bold", size=3.5) +
    theme_minimal() +
    scale_x_continuous(breaks = 2019:2023) +
    expand_limits(y = c(min(var_y)*0.9, max(var_y)*1.15)) + # Donne de l'air en haut et bas
    labs(title=titre, x="Année", y="Effectif")
}

p1 <- style_graph(ggplot(df_viz, aes(x=Annee, y=Naissances)), "Effectif Naissances Totales HDF", df_viz$Naissances)
p2 <- style_graph(ggplot(df_viz, aes(x=Annee, y=Mortinatalite)), "Effectif Mortinatalité HDF", df_viz$Mortinatalite)
p3 <- style_graph(ggplot(df_viz, aes(x=Annee, y=Prematures)), "Effectif Naissances Prématurés HDF", df_viz$Prematures)

grid.arrange(p1, p2, p3, ncol=1)

# --- 6. EXPORT ---
write_xlsx(base_journaliere, "Base_HDF_Journaliere.xlsx")
cat("\n✅ Succès ! Cn")