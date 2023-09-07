##############
# Maïwen YVIN
# 04/09/2023
# Analyse globale (ACP, corrélations, etc.)
##############

# Ouverture du dossier
# setwd("E:/OFB/2023_Rang_Zéro_YVIN_MAIWEN/Analyse")


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

horizons_sondages = read_xlsx("../1_nb_horizons_par_site.xlsx")
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
# on garde que les valeurs quanti pour ACP +
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

# trop de donnees, test en enlevant des donnees peu intéressantes ou incertaines

donnees_pour_ACP2 <- donnees_pour_ACP %>% 
  dplyr::select(-c(Prof_arret,
                   Prof_nappe,
                   Nb_horizons,
                   perimet_BV_m,    
                   Gravelius,
                   Longueur_ecoul,
                   Alt_min_max,     
                   Larg_mouil_moy,
                   Nb_type_granulo,
                   Nb_type_granulo_acc,
                   Ratio_L_H)) %>%
  rename("Largeur_FDV" = Larg_fdv,
         "Surface_ZH" = Surface_ZH_m2,
         "Profondeur_MO" = Prof_MO,
         "Hpb" = Hpb_moy,
         "Lpb" = Lpb_moy,
         "Nombre_CT" = Nb_ct,
         "Longueur_CT" = Longueur_tot_ct,
         "Conductivite" = Conductivi,
         "Surface_BV" = aire_BV_m2,
         "Allongement_BV" = Allonge_BV,
         "Altitude" = Alt_min_min)


# ACP et corrélations

acp2 <- PCA(donnees_pour_ACP2, scale.unit = TRUE, graph = FALSE)

fviz_pca(acp2,
         repel=TRUE,
         addEllipses = TRUE)

fviz_pca_ind(acp2,
             repel=TRUE,
             addEllipses = TRUE)

fviz_pca_var(acp2,
             repel=TRUE)

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

corrplot(matrice_correl, order="hclust", tl.col="black", tl.srt=45, addrect = 5)

# corrplot(matrice_correl, method = "circle", tl.col = "black")
# corrplot(matrice_correl, type="upper", order="AOE", tl.col="black", tl.srt=45)


# # essais modèles linéaires avant automatisation
# 
# ML1 <- lm(Longueur_tot_ct ~ Pente_R0, data = donnees_simplif)
# summary(ML1)
# 
# ML4 <- lm(Longueur_tot_ct ~ Surface_ZH_m2, data = donnees_simplif)
# summary(ML4)
# 
# # Sélection basée sur l'AIC
# library(MASS)
# 
# # Modèle initial avec toutes les variables explicatives
# modele_initial <- lm(Longueur_tot_ct ~ ., data = donnees_pour_ACP2[,-c(10,14,15,16,19,21,22)])
# 
# # Sélection AIC "both"
# modele_final_aic <- stepAIC(modele_initial, direction = "both", trace = FALSE)
# summary(modele_final_aic)
# 
# # Sélection descendante
# modele_final_backward <- stepAIC(modele_initial, direction = "backward")
# summary (modele_final_backward)
# 
# ML5 <- lm(Longueur_tot_ct ~ Pente_R0 + Prof_arret, data = donnees_pour_ACP2)
# summary(ML5)



##########
#### modeles automatisation calcul modele lineaire
##########


# ML6 <- lm(Longueur_tot_ct ~ ACP_pente + aire_BV_m2 + Allonge_BV, data = donnees_pour_ACP3)
# summary(ML6)
# 
# # Modèle initial avec toutes les variables explicatives
# modele_initial <- lm(Longueur_tot_ct ~ ACP_pente + aire_BV_m2 + Allonge_BV, data = donnees_pour_ACP3)
# 
# # Sélection AIC "both"
# modele_final_aic <- stepAIC(modele_initial, direction = "both", trace = FALSE)
# summary(modele_final_aic)

source(file="R/lm_indices_env.R")

library(flextable)
library(purrr)
library(tibble)


# essai en regoupant les pentes (sauf pente_long_BV)

ACP_pente_R0 <- PCA(donnees_pour_ACP2 %>% dplyr::select(Pente_R0, Pente_R1, Pente_lat_SIG, Pente_lat_PTV), scale.unit = TRUE, graph = FALSE) # garde que variables pente
fviz_pca(ACP_pente_R0,
         repel=TRUE,
         addEllipses = TRUE)
ACP_pente_R0 <- ACP_pente_R0$ind$coord[,1]


donnees_pour_ACP4 <- donnees_pour_ACP2 %>%
  dplyr::select(-c(Pente_R0, Pente_R1, Pente_lat_SIG, Pente_lat_PTV)) %>%
  cbind(ACP_pente_R0)

lm_indices_env(
  df = donnees_pour_ACP4,
  variables_dependantes = c("Largeur_FDV",
                            "Surface_ZH",
                            "Profondeur_MO",
                            "pH",
                            # "Conductivite",
                            "Hpb",
                            "Lpb",
                            "Nombre_CT",
                            "Longueur_CT"),
  variables_explicatives = c("Pente",
                             "Surface_BV",
                             # "Allongement_BV",
                             "Altitude",
                             "Longitude",
                             "Latitude"
  ),
  step = FALSE # ne peut être TRUE que si tous les modèles simplifiés par stepAIC retiennent les mêmes variables
) %>%
  rownames_to_column("Variable explicative") %>%
  flextable::flextable() %>%
  flextable::set_table_properties(layout = "autofit", width = .9) %>%
  flextable::theme_zebra()



# essai avec toutes les données centrées réduites

Pente <- PCA(donnees_pour_ACP2 %>% dplyr::select(Pente_R0, Pente_R1, Pente_lat_SIG, Pente_lat_PTV), scale.unit = TRUE, graph = FALSE) # garde que variables pente
Pente <- Pente$ind$coord[,1]

donnees_scale_ACP4 <- donnees_pour_ACP2 %>%
  dplyr::select(-c(Pente_R0, Pente_R1, Pente_lat_SIG, Pente_lat_PTV)) %>%
  cbind(Pente)

         
donnees_scale_ACP4[] <- scale(donnees_scale_ACP4)

print(class(donnees_scale_ACP4))


lm_indices_env(
  df = donnees_scale_ACP4,
  variables_dependantes = c("Largeur_FDV",
                            "Surface_ZH",
                            "Profondeur_MO",
                            "pH",
                            "Hpb",
                            "Lpb",
                            "Nombre_CT",
                            "Longueur_CT"
                            ),
  variables_explicatives = c("Pente",
                             "Surface_BV",
                             # "Allongement_BV",
                             "Altitude",
                             "Longitude",
                             "Latitude"
                             ),
  step = FALSE # ne peut être TRUE que si tous les modèles simplifiés par stepAIC retiennent les mêmes variables
) %>%
  rownames_to_column("Variable explicative") %>%
  flextable::flextable() %>%
  flextable::set_table_properties(layout = "autofit", width = .9) %>%
  flextable::theme_zebra()


# ML apres avoir sorti le tableau

ML_FDV <- lm(Largeur_FDV ~ Pente + Surface_BV + 0, data = donnees_scale_ACP4)
summary(ML_FDV)

ML_ZH <- lm(Surface_ZH ~ Pente + 0, data = donnees_scale_ACP4)
summary(ML_ZH)

ML_MO<- lm(Profondeur_MO ~ Pente + Altitude + Longitude + 0, data = donnees_scale_ACP4)
summary(ML_MO)

ML_PH <- lm(pH ~ Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
summary(ML_PH)

ML_HPB <- lm(Hpb ~ Longitude + Latitude + 0, data = donnees_scale_ACP4)
summary(ML_HPB)

ML_LPB <- lm(Lpb ~ Longitude + 0, data = donnees_scale_ACP4)
summary(ML_LPB)

ML_NB_CT <- lm(Nombre_CT ~ Altitude + 0, data = donnees_scale_ACP4)
summary(ML_NB_CT)

ML_Long_CT <- lm(Longueur_CT ~ Pente + 0, data = donnees_scale_ACP4)
summary(ML_Long_CT)




ML_test1 <- lm(Largeur_FDV ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# Sélection AIC "both"
modele_final_test1 <- stepAIC(ML_test1, direction = "both", trace = FALSE)
summary(modele_final_test1)


# ML_test2 <- lm(Surface_ZH ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# # Sélection AIC "both"
# modele_final_test2 <- stepAIC(ML_test2, direction = "both", trace = FALSE)
# summary(modele_final_test2)
# 
# 
# ML_test3 <- lm(Profondeur_MO ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# # Sélection AIC "both"
# modele_final_test3 <- stepAIC(ML_test3, direction = "both", trace = FALSE)
# summary(modele_final_test3)
# 
# 
# ML_test4 <- lm(pH ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# # Sélection AIC "both"
# modele_final_test4 <- stepAIC(ML_test4, direction = "both", trace = FALSE)
# summary(modele_final_test4)


ML_test5 <- lm(Hpb ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# Sélection AIC "both"
modele_final_test5 <- stepAIC(ML_test5, direction = "both", trace = FALSE)
summary(modele_final_test5)

ML_test6 <- lm(Lpb ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# Sélection AIC "both"
modele_final_test6 <- stepAIC(ML_test6, direction = "both", trace = FALSE)
summary(modele_final_test6)

# ML_test7 <- lm(Nombre_CT ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# # Sélection AIC "both"
# modele_final_test7 <- stepAIC(ML_test7, direction = "both", trace = FALSE)
# summary(modele_final_test7)
# 
# ML_test8 <- lm(Longueur_CT ~ Pente + Surface_BV + Altitude + Longitude + Latitude + 0, data = donnees_scale_ACP4)
# # Sélection AIC "both"
# modele_final_test8 <- stepAIC(ML_test8, direction = "both", trace = FALSE)
# summary(modele_final_test8)




# ACP tests

ACP_var_expl <- PCA(donnees_scale_ACP4 %>% dplyr::select(Pente,
                                                         Surface_BV,
                                                         Altitude,
                                                         Longitude,
                                                         Latitude
                                                         
                                                         ), scale.unit = TRUE, graph = FALSE)


k = 4
kmeans_cluster <- kmeans(ACP_var_expl$ind$coord[, 1:2], centers = k)

df_k.means <- data.frame(PC1 = ACP_var_expl$ind$coord[, 1],
           PC2 = ACP_var_expl$ind$coord[, 2],
           Cluster = as.factor(kmeans_cluster$cluster))

# couleurs clusters
cluster_colors <- scales::hue_pal()(k)
ind_colors <- cluster_colors[kmeans_cluster$cluster]

fviz_pca(ACP_var_expl, 
         repel = TRUE,
         col.ind = ind_colors)


# Effectuez la CAH
hclust_cluster <- hclust(dist(ACP_var_expl$ind$coord[, 1:2]))

# Coupez l'arbre de la CAH pour obtenir le nombre de clusters souhaité
k <- 4
ca_hierarchical_clusters <- cutree(hclust_cluster, k)

# Créez un dataframe qui contient les résultats de l'ACP ainsi que les clusters attribués par la CAH
df_cah <- data.frame(PC1 = ACP_var_expl$ind$coord[, 1],
                     PC2 = ACP_var_expl$ind$coord[, 2],
                     Cluster = as.factor(ca_hierarchical_clusters))

# Obtenez les couleurs pour les clusters
cluster_colors <- scales::hue_pal()(k)

# Associez les couleurs aux individus en fonction des clusters attribués par la CAH
ind_colors <- cluster_colors[ca_hierarchical_clusters]

# Créez le graphique d'ACP avec des points colorés pour les individus
fviz_pca(ACP_var_expl, 
         repel = TRUE, 
         col.ind = ind_colors,
         axes = c(3, 4)) +
  theme(legend.position = "none") 


