library(data.table) # lire les csv
library(tidyverse) # manipuler les données
library(tidylog) # log d'information pour chaque traitement

chemin_data <- "Z:/CNSA/data/"
vecteur_annee <- c("2019","2022")

# Chargement des données 
data_19 <- fread(paste0(chemin_data,"cnsa-export-prix-ehpad-2019-brute.csv"),
                 keepLeadingZeros = T,
                 na.strings="\\N")
data_22 <- fread(paste0(chemin_data,"cnsa-export-prix-ehpad-2022-brute.csv"),
                   keepLeadingZeros = T,
                 na.strings = "")
finess_19 <- fread(paste0(chemin_data,"etalab_stock_et_20191231.csv"),
                                 keepLeadingZeros = T,
                   na.strings="")
finess_22 <- fread(paste0(chemin_data,"etalab_stock_et_20221231.csv"),
                   keepLeadingZeros = T,
                   na.strings = "")

# Vérification rapide (type de NA, nombre de colonnes, informations contenues)
head(data_19) |> view()
head(finess_19) |> view()
head(data_22) |>  view()
head(finess_22) |>  view()

# Vérification des clés pour l'appariement 

data_19 |> nrow() == data_19 |> distinct(finessEt)  |> nrow()
finess_19 |> nrow() == finess_19 |> distinct(nofinesset)  |> nrow()
data_22 |> nrow() == data_22 |> distinct(finessEt)  |> nrow()
finess_22 |> nrow() == finess_22 |> distinct(nofinesset)  |> nrow()

data_19 |> group_by(str_length(finessEt)) |> summarise(n())
finess_19 |> group_by(str_length(nofinesset)) |> summarise(n())
data_22 |> group_by(str_length(finessEt)) |> summarise(n())
finess_22 |> group_by(str_length(nofinesset)) |> summarise(n())

# Merge

data_19 <- data_19 |> 
  left_join(finess_19,
            by = c("finessEt" = "nofinesset"))

data_22 <- data_22 |> 
  left_join(finess_22,
            by = c("finessEt" = "nofinesset"))

# Correction des données 
data_19 <- data_19 |> 
  mutate(prixHebPermCs = as.numeric(str_replace(prixHebPermCs,  ",",".")))

data_22 <- data_22 |> 
  mutate(prixHebPermCs = as.numeric(str_replace(prixHebPermCs,  ",",".")))

# Tableaux 

# Par dep
data_19 |> 
  filter(!is.na(prixHebPermCs)) |> 
  group_by(departement) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()

data_22 |> 
  filter(!is.na(prixHebPermCs)) |> 
  group_by(departement) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()

# Par categ

data_19 |> 
  filter(!is.na(prixHebPermCs)) |> 
  group_by(categagretab) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()

data_22 |> 
  filter(!is.na(prixHebPermCs)) |> 
  filter(!is.na(libsph)) |> 
  group_by(libsph) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()

sirene <- fread("X:/HAB-SIASPP/_Nouvelle Chaîne/ancien/StockEtablissement_utf8.csv", keepLeadingZeros = T)

sirene_soft <- sirene |> 
  select(siret, trancheEffectifsEtablissement)

data_19 <- data_19 |> 
  left_join(sirene_soft,
            by = "siret")

data_22 <- data_22 |> 
  left_join(sirene_soft,
            by = "siret")

data_22 |> filter(is.na(trancheEffectifsEtablissement)) |> group_by(rs) |> count() |> collect() |> view()

# Par tranche

data_19 |> 
  filter(!is.na(prixHebPermCs)) |> 
  filter(!is.na(trancheEffectifsEtablissement)) |> 
  
  group_by(trancheEffectifsEtablissement) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()

data_22 |> 
  filter(!is.na(prixHebPermCs)) |> 
  filter(!is.na(trancheEffectifsEtablissement)) |> 
  group_by(trancheEffectifsEtablissement) |> 
  summarise(n(), mean(prixHebPermCs), min(prixHebPermCs), max(prixHebPermCs),
            sd(prixHebPermCs))  |> view()
