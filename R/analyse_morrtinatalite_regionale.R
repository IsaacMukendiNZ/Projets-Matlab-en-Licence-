# --- 1. PRÉPARATION DES OUTILS ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- 2. TRAITEMENT DE LA PREMIÈRE BASE : MORTALITÉ ---
cat("ÉTAPE 1 : Sélectionnez le fichier des TAUX DE MORTALITÉ\n")
base1 <- read_excel(file.choose())
colnames(base1)[1:5] <- c("Annee", "Code", "Region", "Type", "Taux")

df_mortalite <- base1 %>%
  mutate(Taux = as.numeric(gsub(",", ".", as.character(Taux))),
         Annee = as.numeric(as.character(Annee)),
         Code = trimws(as.character(Code))) %>%
  filter(grepl("total", Type, ignore.case = TRUE)) %>% 
  filter(!is.na(Taux))

# --- 3. TRAITEMENT DE LA DEUXIÈME BASE : EFFECTIFS ---
cat("ÉTAPE 2 : Sélectionnez le fichier des NAISSANCES (Effectifs)\n")
base2 <- read_excel(file.choose())
colnames(base2)[1:5] <- c("Annee", "Code", "Region", "Statut", "Effectif")

df_morts_nes <- base2 %>%
  mutate(Effectif = as.numeric(as.character(Effectif)),
         Annee = as.numeric(as.character(Annee)),
         Code = trimws(as.character(Code))) %>%
  filter(grepl("morts-nés", Statut, ignore.case = TRUE))

# --- 4. CALCULS ET CROISEMENT (Fidèle aux Codes) ---
df_final <- df_mortalite %>%
  inner_join(df_morts_nes, by = c("Annee", "Code")) %>%
  mutate(Nombre_Deces_Calcule = (Taux * Effectif) / 1000)

# CALCUL DE LA MOYENNE GLOBALE
moyenne_globale <- mean(df_final$Taux, na.rm = TRUE)

# CALCUL DES ÉCARTS (Signés pour le graphique, Absolus pour l'analyse)
df_final <- df_final %>%
  mutate(Ecart = Taux - moyenne_globale,
         Ecart_Absolu = abs(Taux - moyenne_globale))

# --- 5. AFFICHAGE CONSOLE ---
cat("\n====================================================\n")
cat("MOYENNE GLOBALE DE RÉFÉRENCE :", round(moyenne_globale, 3), "\n")
cat("====================================================\n")

# --- 6. GRAPHIQUES ---

# Graphe 1 : Évolutions linéaires (Historique complet)
p1 <- ggplot(df_final, aes(x = Annee, y = Taux, color = Region.x, group = Region.x)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = moyenne_globale, linetype = "dashed", color = "black") +
  annotate("text", x = min(df_final$Annee), y = moyenne_globale + 0.2, label = "Moyenne", color = "black") +
  labs(title = "Évolution des taux par rapport à la moyenne globale", x = "Année", y = "Taux") +
  theme_minimal()

# Graphe 2 : TOUTES LES RÉGIONS (Dernière année) - EXCÈS ET DÉFICITS
derniere_annee <- max(df_final$Annee)
df_bilan_last <- df_final %>% 
  filter(Annee == derniere_annee) %>%
  arrange(desc(Ecart))

p2 <- ggplot(df_bilan_last, aes(x = reorder(Region.x, -Ecart), y = Ecart, fill = Ecart > 0)) +
  geom_bar(stat = "identity", color = "black") +
  # On utilise deux couleurs : une pour ce qui dépasse, une pour ce qui est en dessous
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "steelblue"), 
                    labels = c("Sous la moyenne", "Au-dessus (Excès)")) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  geom_text(aes(label = round(Ecart, 2)), 
            vjust = ifelse(df_bilan_last$Ecart > 0, -0.5, 1.5), 
            fontface = "bold", size = 3) +
  labs(title = paste("Écarts de mortalité par région en", derniere_annee),
       subtitle = "Zéro = Moyenne globale. Les barres montrent l'éloignement (Excès ou Déficit).",
       x = "Région", y = "Écart (Taux - Moyenne)", fill = "Statut") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)
print(p2)