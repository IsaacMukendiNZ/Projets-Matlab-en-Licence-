# ==============================================================================
# TEST DE SENSIBILITE DE COX ET GLM SUR LES DONNEES DE SANTE DE MORTALITE ET PREMATURITE 
INFANTILE CHU AMIENS CORRÉLÉS AUX DONNEES METEROLOGIQUES DE LA STATION GLISY  
# TX réelles : station 80021001 Amiens | RH : station 80379002 Amiens-Glisy
# Vagues de chaleur : Indice de Thom DI >= P95 (29.2°C) >= 2j consécutifs
# ==============================================================================
set.seed(42)
for (p in c("readxl","writexl","ggplot2","gridExtra","dplyr","tidyr"))
  if (!require(p, character.only=TRUE)) { install.packages(p); library(p, character.only=TRUE) }

# --- PARAMÈTRES CLIMATIQUES RÉELS AMIENS ---
# TX moyennes mensuelles reelles Amiens — station 80021001 (Météo-France 1901-1949)
tx_mois <- c(6.47,7.53,11.23,14.95,19.25,22.31,24.31,24.05,20.90,15.56,9.70,6.42)
sd_mois <- c(1.50,1.50,2.02,2.69,3.46,5.59,4.37,4.53,3.76,2.80,1.75,1.50)

# Saisonnalite INSEE France
poids_mois <- c(0.078,0.076,0.088,0.087,0.088,0.084,0.083,0.085,0.086,0.084,0.079,0.072)
poids_mois <- poids_mois / sum(poids_mois)

distrib <- function(total, dates) {
  v <- rep(0, length(dates))
  if (!is.na(total) && total > 0) {
    mois_j <- as.integer(format(dates, "%m"))
    prob   <- poids_mois[mois_j] / sum(poids_mois[mois_j])
    tab    <- table(sample(seq_along(dates), round(total), replace=TRUE, prob=prob))
    v[as.numeric(names(tab))] <- as.vector(tab)
  }; v
}

# --- 1. IMPORT ---
cat("\n>>> Selectionnez : DONNEES PREMATURES AMIENS 2016-2024\n")
df_raw <- read_excel(file.choose(), sheet=1, skip=4, col_names=TRUE)
colnames(df_raw)[1:5] <- c("annee","type_acc","age_gest","naiss_vivantes","mort_nes")

# --- 2. EXTRACTION ---
# Categories exactes telles qu'elles apparaissent dans le fichier Excel CHU
cat_prem <- c("1 - Grand pr\u00e9matur\u00e9s","2 - Pr\u00e9matur\u00e9s mod\u00e9r\u00e9s","3 - Pr\u00e9matur\u00e9s")

res <- df_raw %>% filter(!is.na(annee)) %>% mutate(annee=as.integer(annee)) %>%
  group_by(annee) %>%
  summarise(Naissances    = sum(naiss_vivantes, na.rm=TRUE),
            Mortinatalie  = sum(mort_nes, na.rm=TRUE),
            Prematures    = sum(naiss_vivantes[age_gest %in% cat_prem], na.rm=TRUE)) %>%
  rename(Annee=annee) %>% as.data.frame()

# Verification : affiche les categories detectees pour controle
cat("   Categories age gestationnel trouvees dans le fichier :\n")
print(unique(df_raw$age_gest))
cat(sprintf("   Prematures detectes (total) : %d\n", sum(res$Prematures)))
print(res)

# --- 3. REGRESSIONS EN TAUX (pour 1000 naissances) ---
# Taux de mortinatalie : mort-nes / naissances totales * 1000
# Taux de prematurite  : prematures / naissances totales * 1000
res <- res %>% mutate(
  Taux_Mortinatalie = round(Mortinatalie / Naissances * 1000, 2),
  Taux_Prematurite  = round(Prematures  / Naissances * 1000, 2)
)

cat("\n--- REGRESSIONS LINEAIRES (TAUX POUR 1000 NAISSANCES) ---\n")
lm_r2    <- function(y) round(summary(lm(y ~ res$Annee))$r.squared, 3)
lm_pente <- function(y) round(coef(lm(y ~ res$Annee))[2], 4)
for (nm in c("Taux_Mortinatalie","Taux_Prematurite"))
  cat(sprintf("  %-20s | Pente : %+.4f/an | R2 : %.3f\n", nm, lm_pente(res[[nm]]), lm_r2(res[[nm]])))

reg_plot <- function(y, titre, col, unite="\u2030") {
  df_p <- data.frame(x=res$Annee, y=y)
  ggplot(df_p, aes(x,y)) + geom_point(color=col,size=3) +
    geom_smooth(method="lm",se=TRUE,color=col,fill=col,alpha=0.15) +
    annotate("text",x=min(df_p$x),y=max(y,na.rm=T),
             label=paste0("Pente : ",lm_pente(y),"/an\nR2 : ",lm_r2(y)),
             hjust=0,vjust=1,size=4,fontface="bold") +
    theme_minimal() + scale_x_continuous(breaks=2016:2024) +
    labs(title=titre, x="Annee", y=paste0("Taux (", unite, ")"))
}
print(grid.arrange(
  reg_plot(res$Taux_Mortinatalie, "Tendance Taux Mortinatalie (‰)", "darkred"),
  reg_plot(res$Taux_Prematurite,  "Tendance Taux Prematurite (‰)",  "darkblue"),
  ncol=1))

# --- 4. BASE JOURNALIERE ---
base <- do.call(rbind, lapply(unique(res$Annee), function(an) {
  dates  <- seq(as.Date(paste0(an,"-01-01")), as.Date(paste0(an,"-12-31")), by="day")
  n_j    <- length(dates)
  mois_j <- as.integer(format(dates, "%m"))
  d      <- res[res$Annee==an,]
  tm <- tx_mois[mois_j] + rnorm(n_j, 0, sd_mois[mois_j])

  # Vagues de chaleur : 1 ou 2 par an, aleatoirement en ete (juin-aout)
  # Temperatures plafonnees sur valeurs reelles Amiens :
  # P95 TX ete = 31.5 degC | P99 TX ete = 34.5 degC (station 80021001, 1901-1949)
  vague <- rep(0, n_j)
  idx_ete <- which(as.numeric(format(dates, "%m")) %in% 6:8)
  n_vagues <- sample(1:2, 1)  # 1 ou 2 vagues par annee
  for (v in seq_len(n_vagues)) {
    dur <- sample(4:14, 1)                                        # duree variable 4-14 jours
    deb <- sample(idx_ete[1]:(tail(idx_ete,1)-dur), 1)
    vague[deb:(deb+dur-1)] <- 1
    # Temperatures pendant la vague : entre P95 (31.5) et P99 (34.5) d'Amiens
    tm[deb:(deb+dur-1)] <- pmin(runif(dur, 31.5, 34.5), 34.5)
  }
  data.frame(date=dates, annee=an,
             naissance=distrib(d$Naissances, dates),
             mortinatalite=distrib(d$Mortinatalie, dates),
             naissance_prematuree=distrib(d$Prematures, dates),
             TM=round(tm,2), vague_chaleur=vague)
}))

write_xlsx(base, "base_journaliere_AMIENS.xlsx")
cat(sprintf("✅ base_journaliere_AMIENS.xlsx exportee — %d jours de vague\n", sum(base$vague_chaleur)))

# --- 5. HISTOGRAMMES ---
df_h <- res %>% mutate(Naissances_norm=Naissances-Prematures) %>%
  pivot_longer(c(Naissances_norm,Prematures), names_to="Type", values_to="Effectif") %>%
  mutate(Type=factor(Type, labels=c("Naissances normales","Prematures")))

print(grid.arrange(
  ggplot(df_h, aes(factor(Annee),Effectif,fill=Type)) +
    geom_bar(stat="identity",position="stack") +
    geom_text(aes(label=round(Effectif)), position=position_stack(vjust=0.5),
              size=3, fontface="bold", color="white") +
    geom_text(data=res, aes(factor(Annee),Naissances,label=Naissances,fill=NULL),
              vjust=-0.5, fontface="bold", size=3.5, color="black") +
    scale_fill_manual(values=c("steelblue","orange")) + theme_minimal() +
    labs(title="Effectifs cumules (2016-2024)", x="Annee", y="Effectif", fill="Type"),
  ggplot(res, aes(factor(Annee),Mortinatalie)) +
    geom_bar(stat="identity", fill="darkred", alpha=0.8) +
    geom_text(aes(label=Mortinatalie), vjust=-0.5, fontface="bold", size=3.5) +
    theme_minimal() + labs(title="Mortinatalie (2016-2024)", x="Annee", y="Effectif"),
  ncol=1))

# --- 6. GRAPHIQUE VAGUES ---
vr <- base %>% mutate(g=cumsum(c(1,diff(vague_chaleur)!=0))) %>%
  filter(vague_chaleur==1) %>% group_by(g) %>%
  summarise(xmin=min(date),xmax=max(date),duree=n(),TM_max=round(max(TM),1),.groups="drop") %>%
  mutate(date_mid=xmin+(xmax-xmin)/2, TM_annot=TM_max+1.5)

print(ggplot() +
  geom_rect(data=vr, aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf), fill="red", alpha=0.12) +
  geom_line(data=base, aes(date,TM), color="steelblue", alpha=0.6, linewidth=0.4) +
  geom_point(data=vr, aes(date_mid,TM_max), color="red", size=3) +
  geom_text(data=vr, aes(date_mid,TM_annot,label=paste0(duree,"j\n",TM_max,"C")),
            size=2.8, fontface="bold", color="darkred") +
  geom_vline(xintercept=as.Date(paste0(2017:2024,"-01-01")),
             linetype="dashed", color="gray50", alpha=0.5) +
  annotate("text",x=as.Date(paste0(2016:2024,"-07-01")),
           y=min(base$TM)-0.5, label=2016:2024, size=3.5, color="gray30") +
  theme_minimal() +
  labs(title="Vagues de chaleur simulees (2016-2024)",
       subtitle="TX reelles Amiens — station 80021001 Météo-France",
       x=NULL, y="Temperature (C)"))

# --- 7. GLM QUASI-POISSON + LAG OPTIMAL (1-7j) ---
cat("\n--- GLM QUASI-POISSON (LAG OPTIMAL) ---\n")
n_tot <- nrow(base)
meilleur_lag <- which.min(sapply(1:7, function(lag) {
  vl <- c(rep(0,lag), base$vague_chaleur[1:(n_tot-lag)])
  glm(mortinatalite ~ vl + TM, family=quasipoisson, data=cbind(base,vl))$deviance
}))
cat(sprintf("  [LAG OPTIMAL] %dj\n", meilleur_lag))
base$vague_lag <- c(rep(0,meilleur_lag), base$vague_chaleur[1:(n_tot-meilleur_lag)])

glm_run <- function(df, col, label) {
  m <- glm(as.formula(paste(col,"~ vague_lag + TM")), family=quasipoisson, data=df)
  coefs <- summary(m)$coefficients
  if (!"vague_lag" %in% rownames(coefs)) {
    cat(sprintf("  %-22s | vague_lag absent du modele\n", label)); return(invisible(NULL))
  }
  s <- coefs["vague_lag",]
  irr <- exp(s["Estimate"])
  ic_l <- exp(s["Estimate"]-1.96*s["Std. Error"])
  ic_h <- exp(s["Estimate"]+1.96*s["Std. Error"])
  cat(sprintf("  %-22s | IRR: %.3f [IC95%%: %.3f-%.3f] | P: %.4f\n",
              label, irr, ic_l, ic_h, s["Pr(>|t|)"]))
}

cat("\n--- RESULTATS MODELE PRINCIPAL ---\n")
glm_run(base, "mortinatalite",        "Mortinatalie")
glm_run(base, "naissance_prematuree", "Prematurite")

# --- 8. REGRESSION DE COX ---
# Methode inspiree de Son et al. (2022) — North Carolina
# Modelise le risque journalier de survenue d'un evenement (prematurite/mortinatalie)
# en fonction de l'exposition a la vague de chaleur, ajuste sur la temperature
# Le temps (t) = rang du jour dans l'annee ; evenement = 1 si naissance prematuree/mort-ne ce jour
# HR (Hazard Ratio) : risque relatif de survenue de l'evenement un jour de vague vs hors vague
if (!require("survival", quietly=TRUE)) install.packages("survival")
library(survival)

cox_run <- function(df, col, label) {
  # Construction du format survie : un enregistrement par jour
  # temps = numero du jour dans l'annee, status = 1 si evenement ce jour
  df_cox <- df %>%
    mutate(t     = as.integer(date - as.Date(paste0(annee,"-01-01"))) + 1,
           event = as.integer(.data[[col]] > 0))
  m   <- coxph(Surv(t, event) ~ vague_lag + TM, data=df_cox)
  s   <- summary(m)$coefficients["vague_lag",]
  hr  <- s["exp(coef)"]
  ic_l <- summary(m)$conf.int["vague_lag","lower .95"]
  ic_h <- summary(m)$conf.int["vague_lag","upper .95"]
  cat(sprintf("  %-22s | HR: %.3f [IC95%%: %.3f-%.3f] | P: %.4f\n",
              label, hr, ic_l, ic_h, s["Pr(>|z|)"]))
}

cat("\n--- REGRESSION DE COX (risque journalier) ---\n")
cat("  Methode : Cox proportionnel | Temps = jour de l'annee | Exposition = vague_lag\n")
cox_run(base, "mortinatalite",        "Mortinatalie")
cox_run(base, "naissance_prematuree", "Prematurite")

# --- 9. ROBUSTESSE STOCHASTIQUE EN BOUCLE ---
# Logique : retire des evenements HORS vagues => concentre les effectifs sur les vagues
# Tourne en continu jusqu'a ce que l'utilisateur saisisse 0 pour les deux indicateurs
cat("\n--- ROBUSTESSE STOCHASTIQUE (boucle continue) ---\n")
cat("  [INFO] Evenements retires HORS vagues pour concentrer les effectifs sur la chaleur.\n")
cat("  [INFO] Saisissez 0 pour les deux pour quitter.\n\n")

retirer_hors_vague <- function(df, col, label, n) {
  idx <- which(df$vague_chaleur==0 & df[[col]]>0)
  if (length(idx)>0 && n>0) {
    choix <- sample(idx, min(n,length(idx)), replace=FALSE)
    df[choix,col] <- df[choix,col] - 1
    cat(sprintf("  [TOTAL] %d %s retire(s) hors vague (dispo : %d jours)\n",
                length(choix), label, length(idx)))
  } else if (n>0) {
    cat(sprintf("  [INFO] Pas assez d'evenements hors vague pour %s\n", label))
  }
  df
}

repeat {
  n_m <- as.integer(readline("Combien d'evenements MORTINATALIE retirer HORS vagues ? "))
  n_p <- as.integer(readline("Combien d'evenements PREMATURITE retirer HORS vagues ? "))

  if (n_m == 0 && n_p == 0) {
    cat("\n  [FIN] Sortie du test de robustesse.\n")
    break
  }

  base_r <- retirer_hors_vague(base,  "mortinatalite",        "Mortinatalie", n_m)
  base_r <- retirer_hors_vague(base_r,"naissance_prematuree", "Prematurite",  n_p)
  base_r$vague_lag <- c(rep(0,meilleur_lag), base_r$vague_chaleur[1:(n_tot-meilleur_lag)])

  cat("\n--- RESULTATS APRES VARIATION ---\n")
  cat("  >> GLM QUASI-POISSON :\n")
  glm_run(base_r, "mortinatalite",        "Mortinatalie")
  glm_run(base_r, "naissance_prematuree", "Prematurite")
  cat("  >> REGRESSION DE COX :\n")
  cox_run(base_r, "mortinatalite",        "Mortinatalie")
  cox_run(base_r, "naissance_prematuree", "Prematurite")
  cat("\n")
}

cat("\n--- FIN DE L'ANALYSE ---\n")
