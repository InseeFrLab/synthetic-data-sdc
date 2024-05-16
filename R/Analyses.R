library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)
library(tidyverse)
library(tictoc)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(abind)

# Import des données -----------------
FILE_KEY_IN_S3 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

res_simul <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET_SIM,
  opts = list("region" = "")
)
str(res_simul, max.level=1)
methodes <- which(names(res_simul) != "original")

mes_modeles <- c("sample", "cart", "ctree", "parametric", "bag", "rf")

n_sim <- 500
num_seed <- 40889
options(max.print = 10000)

# Statistiques
res_simul_empile <- map(res_simul[methodes], \(df_list) df_list %>% imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())

calc_stats_mean_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ mean(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
} 

table_mean_num <- imap(res_simul_empile, \(df, methode) calc_stats_mean_num(df) %>% mutate(methode = methode)) %>% list_rbind() 


calc_org_mean_num <- function(df) {
  df %>% summarise(
      across(where(is.numeric), list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975)
    )))
}

table_org_mean_num <- calc_stats_mean_num(res_simul$original)

calc_stats_sd_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ sd(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}

table_sd_num <- imap(res_simul_empile, \(df, methode) calc_stats_sd_num(df) %>% mutate(methode = methode)) %>% list_rbind() 


calc_stats_cat <- function(df) {
  cat_stats <- df %>%
    select(index_sim, where(is.factor)) %>%
    pivot_longer(cols = -index_sim, names_to = "variable", values_to = "modalites") %>%
    group_by(index_sim, variable, modalites) %>%
    summarise(freq = n(), .groups = "drop") %>%
    group_by(variable, modalites) %>%
    summarise(across(freq, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}

table_cat <- imap(res_simul_empile, \(df, methode) calc_stats_cat(df) %>% mutate(methode = methode)) %>% list_rbind() 


calc_correlation <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
}

table_cor <- map(
  res_simul[methodes],
  \(list_df){
    array_cor <- map(
      list_df,
      \(df){
        calc_correlation(df)
      }
    ) %>% abind::abind(along = 3) 
    list(mean = array_cor %>% apply(MARGIN = 1:2, FUN = mean),
         sd = array_cor %>% apply(MARGIN = 1:2, FUN = sd))
  }
)



calc_mae <- function(liste_res, )
for (i in 1:length(res_simul[methodes])) {
  MSE = 0
  for (j in 1:length(res_simul[[i]])) {
    for (k in 1:length(res_simul[[i]][[j]])) {
      MSE = MSE + mean(abs(res_simul[[i]][[j]][, k] - res_simul$original[, k]))
    }
  }
}











