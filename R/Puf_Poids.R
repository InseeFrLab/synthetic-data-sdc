if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)


# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac <- setdiff(names(puf65), num)
puf65 <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65 <- puf65[, 2:66]
# num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
# fac <- setdiff(names(puf65), num)
puf65 <- puf65 %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

puf_cart <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf_cart.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65_cart <- puf_cart[, 2:66] %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

puf65_ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_ctgan.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65_ctgan <- puf65_ctgan[, 2:66]
puf65_ctgan <- puf65_ctgan %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

puf65_tvae <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_tvae.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65_tvae <- puf65_tvae[, 2:66]
puf65_tvae <- puf65_tvae %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))


# Analyse des pondérations ------


puf65 %>% pull(EXTRIAN) %>% sum()
puf65_cart %>% pull(EXTRIAN) %>% sum()
puf65_ctgan %>% pull(EXTRIAN) %>% sum()
puf65_tvae %>% pull(EXTRIAN) %>% sum()

#pb avec les poids sommes gigantesques
