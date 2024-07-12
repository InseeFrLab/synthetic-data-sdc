library(dplyr)

download.file("https://www.insee.fr/fr/statistiques/fichier/7657353/FD_csv_EEC22.zip", destfile = "data/eec_puf.zip")

# le dictionnaire des variables est disponible ici
# https://www.insee.fr/fr/statistiques/fichier/7657353/EEC%202022%20_%20Dictionnaire%20des%20codes%20_%20Fichier%20detail.pdf

temp <- tempfile()
download.file("https://www.insee.fr/fr/statistiques/fichier/7657353/FD_csv_EEC22.zip",temp)

dir.create("data/")
unzip(temp, exdir="data/")

puf <- data.table::fread(file.path("data", "FD_EEC_2022.csv"))
str(puf)

doc <- data.table::fread(file.path("data", "Varmod_EEC_2022.csv"))
str(doc)

str(puf)
nrow(puf)

table(puf$TRIM)
table(puf$ANNEE)


# Identifiant des logements
puf %>% count(IDENT, TRIM) %>% filter(n > 4)
length(unique(puf$IDENT))
puf %>% group_by(IDENT, TRIM) %>%
  summarise(nb_pers = n(), .groups = "drop") %>%
  group_by(TRIM) %>% 
  summarise(nb_mean = mean(nb_pers))

puf %>% filter(TRIM == 2, IDENT == "JYQH0PXN") %>% View()

# AGE6
table(puf$AGE6, useNA = "always")

# Variable AAC
table(puf$AAC, useNA = "always")
table(puf$AAC, puf$ACTEU, useNA = "always")

# NA
mean(is.na(puf)) #48% de NA

# Variables aevc bcp de modalités

nb_mod <- purrr::map(puf, \(x) length(unique(x))) %>% purrr::list_c()
names(nb_mod) <- names(puf)
cbind(nb_mod)

# Variables Candidates comme variables continues

hist(puf$HEFFEMP)
table(puf$HEFFEMP, useNA = "always") # => NA

hist(puf$HEFFTOT)
table(puf$HEFFTOT, useNA = "always") # => NA



