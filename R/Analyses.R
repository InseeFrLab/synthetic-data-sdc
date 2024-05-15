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

num = c("age", "depress", "nofriend", "height", "weight", "bmi")
fac = c("sex", "agegr", "placesize", "region", "edu", "eduspec", "socprof", "marital", "ls", "trust", "trustfam", "trustneigh", "sport", "smoke", "alcsol", "wkabint", "englang")

mes_modeles <- c("sample", "cart", "ctree", "parametric", "bag", "rf")

n_sim <- 500
num_seed <- 40889
options(max.print = 10000)

# Statistiques
res_simul_empile <- map(res_simul[methodes], \(df_list) df_list %>% imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())

calc_stats_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ mean(.)
      , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
} 

table_stats = imap(res_simul_empile, \(df, methode) calc_stats_num(df) %>% mutate(methode = methode)) %>% list_rbind() 

# Calcul des statistiques
stats_all_meth <- imap(
  res_simul[methodes], 
  \(res_one_methode, nom_methode) {
    plan(multisession, workers = 10)
    
    res <- future_map(
      res_one_methode, 
      calc_stats, 
      .progress = TRUE
    ) %>% list_rbind() %>% summarise(across(everything(), mean))
    
    plan(sequential)
    
    res <- res %>%
      mutate(method = nom_methode)
    
    return(res)
  },
  .progress = TRUE
) %>% list_rbind()

print(stats_all_meth)

stats_moy = c("min", "max", "mean", "median", "sd", "q1", "q3", "q025", "q975")

for (i in 1:length(mes_modeles)) {
  for (j in 1:length(stats_moy)) {
    stat = 0
    for (k in seq(j, length(stats_all_meth) - 1, 9)) {
      stat = stat + stats_all_meth[i, k]
    }
    stats_all_meth[i, length(stats_all_meth) + j]
  }
}


moy_moy = 0
for (i in seq(3,length(stats_all_meth)-1,9)) {
  moy_moy = moy_moy + stats_all_meth[1, i]
}



,
across(where(is.factor), list(
  min_n = ~ min(table(.)),
  max_n = ~ max(table(.)),
  mean_n = ~ mean(table(.)),
  sd_n = ~ sd(table(.)),
  median_n = ~ median(table(.)),
  q1_n = ~ quantile(table(.), 0.25),
  q3_n = ~ quantile(table(.), 0.75),
  q025_n = ~ quantile(table(.), 0.025),
  q975_n = ~ quantile(table(.), 0.975)
), .names = "{.col}_{.fn}")