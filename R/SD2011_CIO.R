if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

df_tvae <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "df1_tvae_best.csv",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)

df_ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "df1_ctgan_best.csv",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)

# Jeux de donnés ---------------------------------------------------------------
df <- data$original %>%
  select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol"))

df_cart <- data$cart[[1]] %>%
  select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol"))

df_tvae <- df_tvae[, 2:23] %>%
  select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol")) %>%
  mutate_if(is.character, as.factor)

df_tvae <- as.data.frame(df_tvae)

df_ctgan <- df_ctgan[, 2:23] %>%
  select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol")) %>%
  mutate_if(is.character, as.factor) 

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
model.ods <- glm(alcsol ~ ., data = df, family = binomial)
model.sds_cart <- glm.synds(alcsol ~ ., data = syn_cart, family = binomial)
model.sds_ctgan <- glm.synds(alcsol ~ ., data = syn_ctgan, family = binomial)
model.sds_tvae <- glm.synds(alcsol ~ ., data = syn_tvae, family = binomial)

compare(model.sds_cart, df)
compare(model.sds_ctgan, df)
compare(model.sds_tvae, df)

