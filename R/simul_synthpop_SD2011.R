library(synthpop)
library(dplyr)
source("R/fonctions/creation_jeu.R")


# Jeu de données
df <- jeudedonnees_SD2011()
visit_sequence <- c(11,16,22,2,23,24,18,19,1,17,15,21,12,13,14,20,6,3,9,4,10,8,5,7)
regles = list(marital = "age < 18")
regles_val = list(marital = "SINGLE")

visit_sequence = c(5,2,22,23,24,16,11,1,18,19,17,15,21,12,13,14,20,6,3,9,4,10,8)

# Initialisation
mes_modeles <- c("cart","ctree","parametric", "rf")

n_sim <- 1
num_seed <- 1234


# Simulations (cart, ctree, rf, parametric)
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

## Sauvegarde 

BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

# Les fichiers du bucket 
aws.s3::get_bucket(BUCKET, region = "")
aws.s3::put_bucket(BUCKET_SIM, region = "")

date = format(Sys.Date(), "%Y%m%d")
FILE_KEY_OUT_S3 = paste0(date, "_sim_synthpop_cart_ctree_rf_parametric_500_sims.RDS")

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