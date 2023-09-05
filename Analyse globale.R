##############
# Maïwen YVIN
# 04/09/2023
# Analyse globale (ACP, corrélations, etc.)
##############

# Ouverture du dossier
setwd("E:/OFB/2023_Rang_Zéro_YVIN_MAIWEN/Analyse")


# Import de tous les trucs utiles aux analyses
library(esquisse)
library(plyr)
library(stringr)
library(readxl)
library(openxlsx)
library(ggplot2)
library("reshape2") 
library(writexl)
library(gridExtra)
library(emmeans)
library(ggsignif)
library(gplots)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(openxlsx)
library(stats)
library(dplyr)

# Importation des donnees

horizons_sondages = read_xlsx("1_nb_horizons_par_site.xlsx")
BV = read_xlsx("2_BV.xlsx")
mesures_ct = read_xlsx("3_mesures_ct_par_site.xlsx")
total_ct = read.xlsx("4_ct_par_site.xlsx")
pentes = read.xlsx("Pentes_sites_r0.xlsx")
fdv = read.xlsx("Largeur_fdv_propre.xlsx")
geologie = read.xlsx("5_geologie.xlsx")
pedologie = read.xlsx("pedologie_simplif.xlsx")
texture_dominante = read.xlsx("6_texture_dom.xlsx")
zh = read.xlsx("Surface_ZH_FDV.xlsx")
exutoires = read.xlsx("7_exutoires.xlsx")

# changement nom colonne de jointure

texture_dominante <- rename(texture_dominante, ID_site=ID_point)
pedologie <- rename(pedologie, ID_site=ID_point)
exutoires <- rename(exutoires, ID_site=ID_point)


# méga tableau de données

donnees <- geologie %>% 
  left_join(pedologie, by = "ID_site") %>% 
  left_join(horizons_sondages, by = "ID_site") %>% 
  left_join(texture_dominante, by = "ID_site") %>%
  left_join(BV, by = "ID_site") %>% 
  left_join(pentes, by = "ID_site") %>% 
  left_join(fdv, by = "ID_site") %>%
  left_join(zh, by = "ID_site") %>% 
  left_join(mesures_ct, by = "ID_site") %>% 
  left_join(total_ct, by = "ID_site") %>%
  left_join(exutoires, by = "ID_site")

write.xlsx(donnees, file = "donnees_sites_r0.xlsx", rowNames = FALSE)

donnees_simplif = read_xlsx("donnees_simplif_sites_r0.xlsx")


# ATTENTION :
# on garde que les valeurs quali pour ACP +
# remplacement de tous les NA par la moyenne de la colonne
# passage de la colonne ID_site en nom de ligne

donnees_pour_ACP <- donnees_simplif[,-c(2,5,8,22,28,29,34)]
donnees_pour_ACP <- donnees_pour_ACP %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))


donnees_pour_ACP <- as.data.frame(donnees_pour_ACP)
rownames(donnees_pour_ACP) <- donnees_pour_ACP[, 1]
donnees_pour_ACP <- donnees_pour_ACP[, -1]

acp <- PCA(donnees_pour_ACP, scale.unit = TRUE, graph = FALSE)
fviz_pca_ind(acp, repel = TRUE)
fviz_pca_var(acp, repel = TRUE)

fviz_pca(acp,
         repel=TRUE)

# trop de donnees, test en enlevant des donnees peu intéressantes

donnees_pour_ACP2 <- donnees_pour_ACP[,-c(6,9,10,11,22,23,24,28)]


# ACP et corrélations

acp2 <- PCA(donnees_pour_ACP2, scale.unit = TRUE, graph = FALSE)

fviz_pca(acp2,
         repel=TRUE,
         addEllipses = TRUE)

cos2_variables <- acp2$var$cos2
cos2_individus <- acp2$ind$cos2
eigen <- acp2$eig


barplot(eigen[,2],
        xlab = "Dimension",
        ylab = "Pourcentage de Variance Expliqué",
        col = "skyblue",
        border = "black"
)


fviz_pca(acp2,axes = c(1,4),
         addEllipses = TRUE)

matrice_correl <- cor(donnees_pour_ACP2)
corrplot(matrice_correl, method = "circle", tl.col = "black")

corrplot(matrice_correl, type="upper", order="hclust", tl.col="black", tl.srt=45)


# modèles linéaires

ML1 <- lm(Longueur_tot_ct ~ Pente_R0, data = donnees_simplif)
summary(ML1)

# ML2 <- lm(Longueur_tot_ct ~ Pente_R0 + Larg_fdv, data = donnees_simplif)
# summary(ML2)
# pas significatif

# ML3 <- lm(Longueur_tot_ct ~ Pente_R0 + Surface_ZH_m2, data = donnees_simplif)
# summary(ML3)
# pas significatif

ML4 <- lm(Longueur_tot_ct ~ Surface_ZH_m2, data = donnees_simplif)
summary(ML4)



# Sélection basée sur l'AIC
library(MASS)


# Modèle initial avec toutes les variables explicatives
modele_initial <- lm(Longueur_tot_ct ~ ., data = donnees_pour_ACP2[,-c(10,14,15,16,19,21,22)])

# Sélection AIC "both"
modele_final_aic <- stepAIC(modele_initial, direction = "both", trace = FALSE)
summary(modele_final_aic)

# Sélection descendante
modele_final_backward <- stepAIC(modele_initial, direction = "backward")
summary (modele_final_backward)

ML5 <- lm(Longueur_tot_ct ~ Pente_R0 + Prof_arret, data = donnees_pour_ACP2)
summary(ML5)

# Sélection ascendante : NUL CETTE MEHODE !!
# modele_final_forward <- stepAIC(modele_initial, direction = "forward")
# summary (modele_final_forward)

