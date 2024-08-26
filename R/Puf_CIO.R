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
num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac <- setdiff(names(puf65), num)
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
puf_cart <- puf_cart[, 2:66]
puf_cart <- puf_cart %>%
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

# Jeux de donnés ---------------------------------------------------------------
df <- puf65 %>%
  select(c("SEXE", "AGE6", "DIP7", "COUPL_LOG", "ENFRED", "TYPLOG5"))

df_cart <- puf_cart %>%
  select(c("SEXE", "AGE6", "DIP7", "COUPL_LOG", "ENFRED", "TYPLOG5"))
df_cart <- as.data.frame(df_cart)

df_tvae <- puf65_tvae %>%
  select(c("SEXE", "AGE6", "DIP7", "COUPL_LOG", "ENFRED", "TYPLOG5"))
df_tvae <- as.data.frame(df_tvae)

df_ctgan <- puf65_ctgan %>%
  select(c("SEXE", "AGE6", "DIP7", "COUPL_LOG", "ENFRED", "TYPLOG5"))
df_ctgan <- as.data.frame(df_ctgan)

# Synthétisations --------------------------------------------------------------
syn_cart <- syn(df_cart,
                seed = 1)

syn_ctgan <- syn(df_ctgan,
                 seed = 1)
syn_ctgan$syn <- df_ctgan

syn_tvae <- syn(df_tvae,
                seed = 1)
syn_tvae$syn <- df_tvae

# CIO --------------------------------------------------------------------------
model.ods <- glm(ENFRED ~ ., data = df, family = binomial)
model.sds_cart <- glm.synds(ENFRED ~ ., data = syn_cart, family = binomial)
model.sds_ctgan <- glm.synds(ENFRED ~ ., data = syn_ctgan, family = binomial)
model.sds_tvae <- glm.synds(ENFRED ~ ., data = syn_tvae, family = binomial)

compare(model.sds_cart, df)
compare(model.sds_ctgan, df)
compare(model.sds_tvae, df)

