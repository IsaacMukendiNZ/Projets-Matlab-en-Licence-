# ==============================================================================
# SCRIPT FINAL HDF : ANALYSES COMPLETES AVEC INTERPRÉTATIONS STATISTIQUES
# ==============================================================================
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra") 
if (!require("dplyr")) install.packages("dplyr")

library(readxl)
library(writexl)
library(ggplot2)
library(gridExtra)
library(dplyr)

# --- 1. IMPORTATION DES DONNÉES SOURCES ---
cat("\n--- ÉTAPE 1 : IMPORTATION DES DONNÉES ---")

cat("\n[ACTION] Sélectionnez le fichier : EFFECTIFS DES NAISSANCES\n")
f_eff  <- file.choose()
df_eff  <- read_excel(f_eff,  sheet = 2, col_names = FALSE, .name_repair = "minimal")

cat("[ACTION] Sélectionnez le fichier : TAUX DE MORTINATALITÉ\n")
f_mort <- file.choose()
df_mort <- read_excel(f_mort, sheet = 2, col_names = FALSE, .name_repair = "minimal")

cat("[ACTION] Sélectionnez le fichier : TAUX DE PRÉMATURITÉ\n")
f_prem <- file.choose()
df_prem <- read_excel(f_prem, sheet = 2, col_names = FALSE, .name_repair = "minimal")

# --- 2. CALCULS ET CRÉATION DE LA BASE JOURNALIÈRE ---
ligne_hdf_eff  <- which(df_eff[[2]] == "Hauts-de-France")[1] 
ligne_hdf_mort <- which(df_mort[[1]] == "Hauts-de-France")[1]
ligne_hdf_prem <- which(df_prem[[2]] == "Hauts-de-France")[1]

res_final <- data.frame()
for (i in 0:11) {
  annee <- 2012 + i
  idx_tx_m <- 2 + (i * 4); idx_ef_m <- idx_tx_m + 3
  idx_nat_tot <- 3 + (i * 2)   
  col_taux_prem <- 17 + (i * 16); col_eff_prem <- 18 + (i * 16)
  
  if (idx_tx_m <= ncol(df_mort) & idx_nat_tot <= ncol(df_eff)) {
    val_tx_M  <- as.numeric(df_mort[ligne_hdf_mort, idx_tx_m])
    val_ef_M  <- as.numeric(df_mort[ligne_hdf_mort, idx_ef_m])
    val_nat_Y <- as.numeric(df_eff[ligne_hdf_eff, idx_nat_tot])
    val_prem_calc <- if (col_eff_prem <= ncol(df_prem)) {
      round((as.numeric(df_prem[ligne_hdf_prem, col_taux_prem]) * as.numeric(df_prem[ligne_hdf_prem, col_eff_prem])) / 100, 1)
    } else { NA }
    
    res_final <- rbind(res_final, data.frame(
      Annee = annee, 
      Naissances = val_nat_Y,
      Mortinatalité = round((val_ef_M * val_tx_M) / 100, 1),
      Prématurés = ifelse(is.na(val_prem_calc), 0, val_prem_calc)
    ))
  }
}

base_journaliere <- data.frame()
annees_simu <- intersect(2019:2023, res_final$Annee)
for (an in annees_simu) {
  dates <- seq(as.Date(paste0(an, "-01-01")), as.Date(paste0(an, "-12-31")), by="day")
  n_j <- length(dates); data_an <- res_final[res_final$Annee == an, ]
  distrib <- function(total, n) {
    v <- rep(0, n); if(!is.na(total) && total > 0) {
      indices <- sample(1:n, round(total), replace = TRUE)
      tab <- table(indices); v[as.numeric(names(tab))] <- as.vector(tab)
    }; return(v)
  }
  tm <- round(runif(n_j, 12, 15), 2); vague <- rep(0, n_j)
  idx_ete <- which(as.numeric(format(dates, "%m")) %in% 6:8)
  for(v in 1:sample(1:3, 1)) {
    duree <- sample(4:11, 1); debut <- sample(idx_ete[1]:(idx_ete[length(idx_ete)] - duree), 1)
    vague[debut:(debut + duree - 1)] <- 1; tm[debut:(debut + duree - 1)] <- tm[debut:(debut + duree - 1)] + runif(duree, 8, 14)
  }
  base_journaliere <- rbind(base_journaliere, data.frame(
    date = dates, annee = an, naissance = distrib(data_an$Naissances, n_j),
    mortinatalite = distrib(data_an$Mortinatalité, n_j),
    naissance_prematuree = distrib(data_an$Prématurés, n_j),
    TM = tm, vague_chaleur = vague
  ))
}

write_xlsx(base_journaliere, "Base_HDF_Journaliere.xlsx")
cat("\n✅ La base créée a été importée avec succès.\n")

# --- 3. BILAN DES VAGUES DE CHALEUR ---
cat("\n--- RÉCAPITULATIF DES VAGUES DE CHALEUR (2019-2023) ---\n")
analyse_vagues <- base_journaliere %>%
  mutate(vague_group = cumsum(vague_chaleur == 1 & lag(vague_chaleur, default = 0) == 0)) %>%
  filter(vague_chaleur == 1) %>%
  group_by(vague_group) %>%
  summarise(
    Debut = min(date), 
    Fin = max(date), 
    Jours = n(), 
    TM_Max = max(TM),
    Naiss_Totales = sum(naissance), 
    Prématurés = sum(naissance_prematuree), 
    Mortinatalité = sum(mortinatalite),
    .groups = 'drop'
  ) %>% select(-vague_group) # L'année n'est plus groupée ni affichée ici

print(as.data.frame(analyse_vagues))

# --- 4. ANALYSE STATISTIQUE (GLM QUASI-POISSON) ---
cat("\n--- ANALYSE STATISTIQUE (GLM QUASI-POISSON) ---\n")
res_stats <- data.frame()

analyser_glm <- function(colonne, label) {
  modele <- glm(as.formula(paste(colonne, "~ vague_chaleur + TM")), family = quasipoisson, data = base_journaliere)
  res <- summary(modele)
  
  coef_vague <- res$coefficients["vague_chaleur", "Estimate"]
  se_vague   <- res$coefficients["vague_chaleur", "Std. Error"]
  irr        <- exp(coef_vague)
  pval       <- res$coefficients["vague_chaleur", "Pr(>|t|)"]
  disp       <- res$dispersion
  
  cat(paste0("\n> Indicateur : ", label, "\n"))
  cat(sprintf("  Facteur Dispersion : %.2f", disp))
  cat(if(disp > 1.2) " -> Surdispersion détectée. Correction appliquée.\n" else " -> Données stables.\n")
  
  cat(sprintf("  Risque (IRR) : %.3f | P-value : %.4f\n", irr, pval))
  
  if(pval < 0.05) {
    pourcent <- round((irr - 1) * 100, 1)
    cat(paste0("  -> INTERPRÉTATION : Effet significatif. Augmentation du risque de ", pourcent, "%.\n"))
  } else {
    cat("  -> INTERPRÉTATION : Pas d'effet statistiquement significatif (p >= 0.05).\n")
  }
  
  return(data.frame(Indicateur = label, IRR = irr, 
                    Inf_95 = exp(coef_vague - 1.96 * se_vague), 
                    Sup_95 = exp(coef_vague + 1.96 * se_vague), P = pval))
}

res_stats <- rbind(analyser_glm("mortinatalite", "Mortinatalité"), 
                   analyser_glm("naissance_prematuree", "Prématurité"))

# --- 5. GRAPHIQUES ---
df_viz <- res_final %>% filter(Annee >= 2019)
style_an <- function(p, titre, var_y) {
  p + geom_line(alpha=0.5, color="gray") + 
    geom_point(color="red", size=2) + 
    geom_text(aes(label=var_y), vjust=-1.5, fontface="bold", size=3) +
    theme_minimal() + 
    labs(title=titre, x="Année", y="Nombre de cas") + 
    scale_x_continuous(breaks = 2019:2023) +
    expand_limits(y = c(min(var_y)*0.8, max(var_y)*1.2))
}

p1 <- style_an(ggplot(df_viz, aes(x=Annee, y=Naissances)), "Naissances Totales", df_viz$Naissances)
p2 <- style_an(ggplot(df_viz, aes(x=Annee, y=Mortinatalité)), "Mortinatalité (Annuel)", df_viz$Mortinatalité)
p3 <- style_an(ggplot(df_viz, aes(x=Annee, y=Prématurés)), "Prématurés (Annuel)", df_viz$Prématurés)

p4 <- ggplot(res_stats, aes(x = Indicateur, y = IRR)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color="red") +
  geom_errorbar(aes(ymin = Inf_95, ymax = Sup_95), width = 0.2) + 
  geom_point(size = 4, color = "darkblue") +
  geom_text(aes(label = paste0("p = ", round(P, 3))), vjust = -2, size = 3.5) + 
  theme_minimal() + 
  labs(title = "Impact Chaleur (IRR & IC95%)", y = "Incident Rate Ratio (IRR)", x="") + 
  coord_flip()

grid.arrange(p1, p2, p3, p4, ncol=1)