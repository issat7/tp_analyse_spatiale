# Chargez les bibliothèques
library(sf)
library(dplyr)

# Étape 1: Importer le fond communal avec l'encodage spécifié
communes <- st_read("commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")

# Étape 2: Résumé/descriptif du contenu
summary(communes)

# Étape 3: Afficher les dix premières lignes et regarder la dernière colonne
View(head(communes, 10))

# Étape 4: Afficher le système de projection
st_crs(communes)

# Étape 5: Créer une table "communes_Bretagne"
communes_Bretagne <- communes %>%
  filter(dep %in% c("22", "29", "35", "56")) %>%
  select(code, libelle, epc, dep, surf)



#EXERCICE 2

#1
install.packages("openxlsx")
library(mapsf)
library(openxlsx)
library(dplyr)

dep_francemetro_2021 <-st_read("dep_francemetro_2021.shp", options="ENCODING=WINDOWS-1252")
tx_pauvrete<-read.xlsx("Taux_pauvrete_2018.xlsx")
dep_francemetro_2021_pauv<-dep_francemetro_2021 %>%
  left_join(tx_pauvrete %>% select(-Dept), by=c("code"="Code"))
