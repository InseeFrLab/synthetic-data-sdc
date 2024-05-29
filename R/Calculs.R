source("R/Analyses.R")
source("R/fonctions/Correlations.R")
source("R/fonctions/Densites.R")
source("R/fonctions/Graphiques.R")
source("R/fonctions/Scores_propension.R")
source("R/fonctions/Utilite.R")
source("R/fonctions/Calculs/MAE.R")
source("R/fonctions/Calculs/MSE.R")
source("R/fonctions/Calculs/Statistiques_categorielles.R")
source("R/fonctions/Calculs/Statistiques_numeriques.R")
source("R/fonctions/Calculs/KS_test.R")

data_empile <- map(data[methodes], \(df_list) df_list %>%
                     imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())

data_empile_combined <- map(mes_modeles, \(model) {
  data_empile[[model]] %>% mutate(model = model)})

data_original <- data$original %>% mutate(model = "original")
data_empile_combined <- bind_rows(data_empile_combined, data_original)

table_mean_num <- imap(data_empile, \(df, methode) calc_stats_mean_num(df) %>%
                         mutate(methode = methode)) %>% list_rbind()

table_org_mean_num <- calc_org_mean_num(data$original)

table_sd_num <- imap(data_empile, \(df, methode) calc_stats_sd_num(df) %>%
                       mutate(methode = methode)) %>% list_rbind() 


table_cat <- imap(data_empile, \(df, methode) calc_stats_cat(df) %>%
                    mutate(methode = methode)) %>% list_rbind()

cor_comp = list()
for (i in 1:length(mes_modeles)) {
  cor_comp[[i]] = abs(table_cor[[i]][[1]] - cor(data$original[, varsnum]))
}
names(cor_comp) <- mes_modeles

somme_cor_mat = matrix(0, nrow = 1, ncol = 6)
for (i in 1:length(mes_modeles)) {
  somme_cor_mat[i] = sum(cor_comp[[i]])
}
colnames(somme_cor_mat) = mes_modeles

table_mae <- calc_mae(data)

table_mse <- calc_mse(data)

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

resultats <- list(data_empile, data_original, data_empile_combined, table_mean_num, table_sd_num, table_cat, table_cor,
                  cor_comp, somme_cor_mat, table_mae, table_mse, reg_coeff,
                  utility_measures_all_meth, nb_repliques_all_meth, KS_test(data))
# Export -----------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")

# Les fichiers du bucket
aws.s3::get_bucket(BUCKET, region = "")
aws.s3::put_bucket(BUCKET_SIM_2, region = "")

FILE_KEY_OUT_S3 = "resultats_analyses.RDS"

aws.s3::s3write_using(
  resultats,
  FUN = saveRDS,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET_SIM_2,
  opts = list("region" = "")
)

print(aws.s3::get_bucket(BUCKET, region = ""))
