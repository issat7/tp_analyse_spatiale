# Chargement des packages
library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)

# Import des données
iris<-st_read("iris_franceentiere_2021.shp")
data<-read.csv2("BASE_TD_FILO_DISP_IRIS_2018.csv",sep=";",dec=".")

# Jointure
marseille<-iris %>% 
  filter(substr(depcom,1,3)=="132") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )

# Installer la bibliothèque si elle n'est pas déjà installée
# install.packages("sf")


# Coordonnées en WGS84 (exemple)
wgs84_coordinates <- st_sfc(st_point(c(lon = 2.3522, lat = 48.8566)), crs = 4326)

# Convertir en Lambert-93 (EPSG:2154)
lambert93_coordinates <- st_transform(wgs84_coordinates, crs = 2154)

# Afficher les résultats
print("Coordonnées en WGS84:")
print(st_coordinates(wgs84_coordinates))

print("Coordonnées en Lambert-93:")
print(st_coordinates(lambert93_coordinates))

# Supposons que votre jeu de données s'appelle "donnees" et contient les colonnes "RevenuMedian" et "Arrondissement"

# Résumé statistique de la variable de revenu médian
summary_stat <- summary(marseille$DISP_MED18)
print("Résumé statistique du revenu médian :")
print(summary_stat)

# Boxplot du revenu moyen en fonction des arrondissements
boxplot_data <- list(x = marseille$depcom, y = marseille$DISP_MED18)
boxplot(boxplot_data$y ~ boxplot_data$x, 
        main = "Boxplot du Revenu Médian par Arrondissement",
        xlab = "Arrondissement",
        ylab = "Revenu Médian")

library(ggplot2)

# Supprimer les valeurs manquantes
donnees_spatiales <- marseille[complete.cases(marseille$DISP_MED18), ]

# Discrétisation automatique des revenus en classes (quartiles ici)
breaks <- quantile(donnees_spatiales$DISP_MED18)
donnees_spatiales$DISP_MED18 <- as.numeric(donnees_spatiales$DISP_MED18)

# Créer la carte
ggplot() +
  geom_sf(data = donnees_spatiales, aes(fill = cut(DISP_MED18, breaks = breaks))) +

  labs(title = "Carte des Revenus Médians à Marseille")

# Supposons que votre jeu de données s'appelle "fond_iris_marseille" et contient la variable DISP_MED18

# Créer une permutation aléatoire des revenus médians par iris
set.seed(123)  # pour la reproductibilité
DISP_MED18_ALEA <- sample(donnees_spatiales)


# Charger les bibliothèques
library(sf)
library(spdep)
library(spatialreg)

# Représenter la distribution géographique de la variable aléatoire
ggplot() +
  geom_sf(data = DISP_MED18_ALEA, aes(fill = DISP_MED18)) +
  scale_fill_viridis_c(name = "Revenu Médian Aléatoire") +
  theme_minimal() +
  labs(title = "Distribution Aléatoire des Revenus Médians par Iris")

# Comparer avec la distribution réelle
ggplot() +
  geom_sf(data = donnees_spatiales, aes(fill = DISP_MED18)) +
  scale_fill_viridis_c(name = "Revenu Médian Réel") +
  theme_minimal() +
  labs(title = "Distribution Réelle des Revenus Médians par Iris")

# Mesurer l'autocorrélation spatiale (indice de Moran)
coords <- st_coordinates()
w <- spdep::st_neighbours(fond_iris_marseille)
moran_test <- moran.test(DISP_MED18_ALEA, w)
print(moran_test)

