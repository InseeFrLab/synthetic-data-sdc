if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)

# Preprocessing ----------------------------------------------------------------
puf <- puf %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  mutate(across(all_of(fac), ~replace_na(as.character(.), "999"))) %>%
  mutate(across(all_of(num), ~replace_na(., -8))) %>%
  mutate_if(is.character, as.factor)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "puf.RDS",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf65 <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf65_cart <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf_cart.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf65_ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_ctgan.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf65_tvae <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_tvae.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

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

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

# Traitement--------------------------------------------------------------------
puf65 <- puf65[, 2:66]
num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac <- setdiff(names(puf65), num)

puf65 <- puf65 %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  as.data.frame

puf65_cart <- puf65_cart[, 2:66]

puf65_cart <- puf65_cart %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  as.data.frame

puf65_ctgan <- puf65_ctgan[, 2:66]

puf65_ctgan <- puf65_ctgan %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  as.data.frame

puf65_tvae <- puf65_tvae[, 2:66]

puf65_tvae <- puf65_tvae %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  as.data.frame


# pMSE -------------------------------------------------------------------------
pMSE_cart <- utility.gen(puf65_cart, puf65, nperms = 1)$pMSE

pMSE_ctgan <- utility.gen(puf65_ctgan, puf65, nperms = 1)$pMSE

pMSE_tvae <- utility.gen(puf65_tvae, puf65, nperms = 1)$pMSE

pMSE_cart1 <- utility.gen(puf65_cart, puf65, nperms = 1)$pMSE

pMSE_ctgan1 <- utility.gen(puf65_ctgan, puf65, nperms = 1)$pMSE

pMSE_tvae1 <- utility.gen(puf65_tvae, puf65, nperms = 1)$pMSE


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

# Répliqués --------------------------------------------------------------------
  # CART
replicated.uniques(syn_cart, df)
# no.unique : 3264, no.replications = 411, per.replications = 8.928959
df_comb_cart <- rbind(df, df_cart)
sum(duplicated(df_comb_cart)) # 2629

  #CTGAN
replicated.uniques(syn_ctgan, df)
# no.unique : 3264, no.replications = 377, per.replications = 8.190311
df_comb_ctgan <- rbind(df, df_ctgan)
sum(duplicated(df_comb_ctgan)) # 2049

  #TVAE
replicated.uniques(syn_tvae, df)
# no.unique : 3264, no.replications = 344, per.replications = 7.473387
df_comb_tvae <- rbind(df, df_tvae)
sum(duplicated(df_comb_tvae)) # 3341


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

# Filtres ----------------------------------------------------------------------
source("~/work/synthetic-data-sdc/R/fonctions/Naf_incorrect.R")
source("~/work/synthetic-data-sdc/R/fonctions/Filtres_puf.R")

# Filtres CART -----------------------------------------------------------------
filtres_puf(puf_cart)

#NAF
naf_incorrect(puf_cart)

# Filtres TVAE -----------------------------------------------------------------
filtres_puf(puf65_tvae)

# NAF
naf_incorrect(puf65_tvae)


# Filtres CTGAN -----------------------------------------------------------------
filtres_puf(puf65_ctgan)

# NAF
naf_incorrect(puf65_ctgan)



tic()
syn_cart <- syn(puf65,
                maxfaclevels = 100,
                seed = 1)
toc()

tic()
pMSE_syn_cart <- utility.gen(syn_cart, puf65, nperms = 1)$pMSE
toc()







