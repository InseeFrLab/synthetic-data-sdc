library(synthpop)
# library(timeR)
library(dplyr)
library(ggplot2)

mes_modeles <- c("cart","ctree","parametric", "rf")

df = iris
visit_sequence = names(iris)
regles = NULL
regles_val = NULL
n_sim = 10
num_seed = 1234

res_simulation <- list(
  meta = list("","","",""),
  data = list(df, df, df, df)
)
names(res_simulation$meta) <- mes_modeles
names(res_simulation$data) <- mes_modeles

list_calcul <- purrr::map(
  mes_modeles,
  \(meth) syn(
    df, method = meth, m = n_sim, 
    visit.sequence = visit_sequence, 
    rules = regles, rvalues = regles_val, 
    models = TRUE, seed = num_seed
  ),
  .progress = TRUE
)
names(list_calcul) <- mes_modeles

res_simulation$meta <- list_calcul %>% 
  purrr::map(  \(calcul) calcul[-3] )

res_simulation$data <- list_calcul %>% 
  purrr::map(  \(calcul) calcul$syn )

res_simulation$data %>% 
  purrr::imap(
    \(methl, nom){
      purrr::imap(
        methl,
        \(dat,num_sim) tibble(meth = nom, num_sim = num_sim, corr_hw = cor(dat$Sepal.Length, dat$Sepal.Width))
      ) %>% purrr::list_rbind()
    }
  ) %>% purrr::list_rbind() %>% 
  group_by(meth) %>% 
  summarise(moy_cor = mean(corr_hw), n = n(), min_corr = min(corr_hw)) %>% 
  print()

## Sauvegarde 

BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

# Les fichiers du bucket 
aws.s3::get_bucket(BUCKET, region = "")
aws.s3::put_bucket(BUCKET_SIM, region = "")

FILE_KEY_OUT_S3 = "20240503_sim_synthpop_all_methodes_10_sims.RDS"

aws.s3::s3write_using(
  res_simulation,
  FUN = saveRDS,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET_SIM,
  opts = list("region" = "")
)

print(aws.s3::get_bucket(BUCKET, region = ""))

# import 
# res_simulation_import <- aws.s3::s3read_using(
#   FUN = readRDS,
#   object = FILE_KEY_OUT_S3,
#   bucket = BUCKET_SIM,
#   opts = list("region" = "")
# )





# res <- purrr::map(seq_along(1:100000), rnorm, n = 100000, .progress = TRUE)









