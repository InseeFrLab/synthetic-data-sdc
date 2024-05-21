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

# Import des données -----------------------------------------------------------
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

data = res_simul
varsnum = c("age", "depress", "nofriend", "height", "weight", "bmi")
num = c("age", "nofriend", "height", "weight", "bmi")
fac = c("sex", "agegr", "placesize", "edu", "socprof", "marital", "ls", "depress", "trust", "trustfam", "trustneigh", "sport", "smoke", "alcabuse", "alcsol", "wkabint", "englang")


# Stats variable numériques ----------------------------------------------------
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

table_sd_num <- imap(res_simul_empile, \(df, methode) calc_stats_sd_num(df) %>%
                       mutate(methode = methode)) %>% list_rbind() 


# Stats variable catégorielles -------------------------------------------------

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

table_cat <- imap(res_simul_empile, \(df, methode) calc_stats_cat(df) %>%
                    mutate(methode = methode)) %>% list_rbind() 


# Correlations -----------------------------------------------------------------

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

cor_comp = list()
for (i in length(mes_modeles)) {
  cor_comp[[i]] = abs(table_cor[[i]][[1]] - cor(data$original[, varsnum]))
}
names(cor_comp) <- mes_modeles

somme_cor_mat = matrix(0, nrow = 1, ncol = 6)
for (i in length(mes_modeles)) {
  somme_cor_mat[i] = sum(cor_comp[[i]])
}
colnames(somme_cor_mat) = mes_modeles

# MAE --------------------------------------------------------------------------

calc_mae <- function(res) {
  mae_table = matrix(0, nrow = 6, ncol = 5)
  
  for (i in 1:length(res[methodes])) {
    mae_table_meth = matrix(0, nrow = 500, ncol = 5)
    
    for (j in 1:length(res[[i]])) {
      factor(res[[i]][[j]][, "depress"])
      for (k in 1:length(fac)) {
        MAE = mean(abs(res$original[[fac[k]]] - res[[i]][[j]][[fac[k]]]))
        mae_table_meth[j, k] = MAE
        
      }
    }
    mae_table[i, ] = colMeans(mae_table_meth)
  }
  
  row.names(mae_table) = mes_modeles
  colnames(mae_table) = fac
  return(mae_table)
}



# Analyse bmi ------------------------------------------------------------------
bmi_comp <- function(res) {
  matrice = matrix(0, nrow = 2, ncol = 6)
  for (i in 1:length(res[methodes])) {
    mat_cal = matrix(0, nrow = 500, ncol = 2)
    for (j in 1:length(res[[i]])) {
      mat_cal[j, 1] = mean(10000 * (res[[i]][[j]][, "weight"] / (res[[i]][[j]][, "height"])^2) - res$original[, "bmi"])
      mat_cal[j, 2] = mean(res[[i]][[j]][, "bmi"] - res$original[, "bmi"])
    }
    matrice[1, i] = sum(mat_cal[, 1])
    matrice[2, i] = sum(mat_cal[, 2])
  }
  
  colnames(matrice) = mes_modeles
  row.names(matrice) = c("Comparaison synth/synth", "Comparaison synth/org")
  return(matrice)
}

reg_coeff <- matrix(0, nrow = 2, ncol = length(mes_modeles))
for (i in 1:length(mes_modeles)) {
  coeff = matrix(0, nrow = length(data[[i]]), ncol = 2)
  for (j in 1:length(data[[i]])) {
    var_test = 10000 * (data[[i]][[j]][, "weight"] / (data[[i]][[j]][, "height"])^2)
    reglin = lm(bmi ~ var_test, data = data[[i]][[j]])
    coeff[j, 1] = reglin$coefficients[1]
    coeff[j, 2] = reglin$coefficients[2]
  }
  reg_coeff[1, i] = mean(coeff[, 1])
  reg_coeff[2, i] = mean(coeff[, 2])
}
row.names(reg_coeff) <- c("intercept", "coeff")
colnames(reg_coeff) <- mes_modeles


# Utilite ----------------------------------------------------------------------

utility_measures_all_meth <- imap(
  data[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        synthpop::utility.gen(
          df_synth,
          data$original, 
          method = "cart",
          resamp.method = "none",
          print.flag = FALSE
        )[c("pMSE","SPECKS","PO50","U")] %>% 
          as_tibble() %>% 
          mutate(method = nom_methode)
      }
    )
    plan(sequential)
    return(res %>% list_rbind())
  },
  .progress = TRUE
) %>% list_rbind()


# Recherche des répliques ------------------------------------------------------

nb_repliques_all_meth <- imap(
  data[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        tibble(
          n_replicats = inner_join(data$original, df_synth, relationship = "many-to-many") %>% nrow(),
          method = nom_methode
        )
      }
    )
    plan(sequential)
    return(res %>% list_rbind())
  },
  .progress = TRUE
) %>% list_rbind()

#806.579 sec elapsed

















