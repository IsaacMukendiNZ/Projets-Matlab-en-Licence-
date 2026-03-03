# --- 1. CONFIGURATION & PACKAGES ---
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Liste des codes DOM à exclure
codes_exclure <- c("01", "02", "03", "04", "06")

# --- 2. CHARGEMENT ET FILTRAGE DES TAUX ---
cat("\n📂 Sélectionnez le fichier des TAUX\n")
path_taux <- file.choose()
df_taux <- read_excel(path_taux) %>%
  setNames(c("Annee", "Code", "Region", "Type", "Taux")) %>%
  mutate(
    Taux = as.numeric(gsub(",", ".", as.character(Taux))),
    Annee = as.numeric(as.character(Annee)),
    Code = trimws(as.character(Code))
  ) %>%
  filter(!is.na(Taux), !(Code %in% codes_exclure)) %>%
  group_by(Annee, Code) %>%
  # On garde la priorité : Totale > Induite > Spontanée
  arrange(factor(Type, levels = c("Totale", "Induite", "Spontanée"))) %>%
  slice(1) %>% 
  ungroup()

# --- 3. CHARGEMENT ET FILTRAGE DES EFFECTIFS ---
cat("\n📂 Sélectionnez le fichier des EFFECTIFS\n")
path_eff <- file.choose()
df_eff <- read_excel(path_eff) %>%
  setNames(c("Annee", "Code", "Region", "Statut", "Valeur")) %>%
  mutate(
    Valeur = as.numeric(as.character(Valeur)),
    Annee = as.numeric(as.character(Annee)),
    Code = trimws(as.character(Code))
  ) %>%
  filter(!(Code %in% codes_exclure))

# Extraction des naissances avec dplyr::select pour éviter les conflits
df_naissances <- df_eff %>% 
  filter(grepl("Naissance", Statut, ignore.case = TRUE)) %>% 
  dplyr::select(Annee, Code, N_Totales = Valeur)

# --- 4. FUSION ET CALCULS FINAUX ---
df_final <- df_taux %>%
  inner_join(df_naissances, by = c("Annee", "Code")) %>%
  mutate(Nb_Morts_Calcules = (Taux * N_Totales) / 1000) %>%
  group_by(Region) %>%
  mutate(
    Moyenne_Reg = mean(Taux, na.rm = TRUE),
    Ecart = Taux - Moyenne_Reg
  ) %>%
  ungroup()

# --- 5. AFFICHAGE CONSOLE ---

cat("\n====================================================\n")
cat("      RÉSULTATS DE MORTINATALITÉ (RÉSUMÉ)\n")
cat("====================================================\n")
resume_stats <- df_final %>%
  group_by(Region) %>%
  summarise(
    Moyenne_Taux = round(mean(Taux), 3),
    Max_Taux = max(Taux),
    Annee_Max = Annee[which.max(Taux)]
  ) %>%
  arrange(desc(Moyenne_Taux))
print(as.data.frame(resume_stats))

cat("\n>>> TEST ANOVA (Variation temporelle) :\n")
print(summary(aov(Taux ~ as.factor(Annee), data = df_final)))

cat("\n>>> TENDANCES PAR RÉGION (Pentes de régression) :\n")
tendances <- df_final %>%
  group_by(Region) %>%
  do(mod = lm(Taux ~ Annee, data = .)) %>%
  mutate(
    Pente = round(summary(mod)$coefficients[2,1], 4),
    P_Value = round(summary(mod)$coefficients[2,4], 4)
  ) %>%
  dplyr::select(Region, Pente, P_Value) %>%
  arrange(Pente)
print(as.data.frame(tendances))

# --- 6. GRAPHIQUES ---

# G1 : Tendance Globale (Linéaire)
p1 <- ggplot(df_final, aes(x = Annee, y = Taux)) +
  geom_point(aes(color = Region), alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(title = "Évolution Globale des Taux de Mortinatalité",
       subtitle = "Ligne de tendance nationale (Hexagone)") +
  theme_minimal()

# G2 : Distribution Annuelle (Boxplot)
p2 <- ggplot(df_final, aes(x = as.factor(Annee), y = Taux, fill = as.factor(Annee))) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Variabilité des Taux par Année", x = "Année", y = "Taux") +
  theme_minimal() + theme(legend.position = "none")

# Affichage des graphiques
print(p1)
print(p2)

# Graphiques d'écarts régionaux (un par région)
cat("\n📊 Génération des graphiques d'écarts régionaux...\n")
for (reg in unique(df_final$Region)) {
  df_reg <- df_final %>% filter(Region == reg)
  p_reg <- ggplot(df_reg, aes(x = Annee, y = Ecart, fill = Ecart > 0)) +
    geom_col(color = "black") +
    scale_fill_manual(values = c("TRUE" = "red3", "FALSE" = "steelblue4")) +
    labs(title = paste("Écarts à la moyenne :", reg), 
         subtitle = "Rouge = Au-dessus de la normale | Bleu = En-dessous",
         y = "Écart de taux", x = "Année") +
    theme_light() + theme(legend.position = "none")
  print(p_reg)
}