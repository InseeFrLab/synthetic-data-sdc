library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)
library(tidyverse)
library(tictoc)
library(ggplot2)
library(viridis)
library(abind)
library(plotly)
library(wesanderson)
library(pracma)
library(hrbrthemes)
library(rmarkdown)
library(knitr)

# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")
FILE_KEY_IN_S3_2 <- "resultats_analyses.RDS"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

analyses <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_2,
  bucket = BUCKET_SIM_2,
  opts = list("region" = "")
)
names(analyses) <- c("data_empile", "data_original", "data_empile_combined", "table_mean_num", "table_sd_num", "table_cat", "table_cor",
                     "cor_comp", "somme_cor_mat", "table_mae", "table_mse", "reg_coeff",
                     "utility_measures_all_meth", "nb_repliques_all_meth", "KS_test")

mes_modeles <- c("sample", "cart", "ctree", "parametric", "bag", "rf")

n_sim <- 500
num_seed <- 40889
options(max.print = 10000)

varsnum = c("age", "depress", "nofriend", "height", "weight", "bmi")
num = c("age", "nofriend", "height", "weight", "bmi")
fac = c("sex", "agegr", "placesize", "edu", "socprof", "marital", "ls", "depress",
        "trust", "trustfam", "trustneigh", "sport", "smoke", "alcabuse", "alcsol", "wkabint", "englang")

# Modifier le type de depress en fonction du besoin
#data$sample[[1]][, "depress"] <- factor(data$sample[[1]][, "depress"], levels = 0:21)
#data$sample[[1]][, "depress"] <- as.numeric(data$sample[[1]][, "depress"])

# data_empile <- map(data[methodes], \(df_list) df_list %>%
#                      imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())
# 
# data_empile_combined <- map(mes_modeles, \(model) {
#   data_empile[[model]] %>% mutate(model = model)})
# 
# data_original <- data$original %>% mutate(model = "original")
# data_empile_combined <- bind_rows(data_empile_combined, data_original)

# Export des résultats ---------------------------------------------------------
# resultats <- list(data_empile, data_original, data_empile_combined, table_mean_num, table_sd_num, table_cat, table_cor,
#                   cor_comp, somme_cor_mat, table_mae, table_mse, reg_coeff,
#                   utility_measures_all_meth, nb_repliques_all_meth)
# 
# BUCKET = "projet-donnees-synthetiques"
# BUCKET_SIM = file.path(BUCKET, "analyses")
# 
# # Les fichiers du bucket
# aws.s3::get_bucket(BUCKET, region = "")
# aws.s3::put_bucket(BUCKET_SIM, region = "")
# 
# date = format(Sys.Date(), "%Y%m%d")
# FILE_KEY_OUT_S3 = "resultats_analyses.RDS"
# 
# aws.s3::s3write_using(
#   resultats,
#   FUN = saveRDS,
#   object = FILE_KEY_OUT_S3,
#   bucket = BUCKET_SIM,
#   opts = list("region" = "")
# )
# 
# print(aws.s3::get_bucket(BUCKET, region = ""))
# 

# Tests ------------------------------------------------------------------------
