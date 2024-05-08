packages <- c("synthpop","dplyr","furrr","purrr","aws.s3")
p <- lapply(packages, \(pack) if(! pack %in% installed.packages()) install.packages(pack))

library(synthpop)
library(dplyr)
library(furrr)
library(purrr)
source("R/fonctions/creation_jeu.R")

# Jeu de données
df <- jeudedonnees_SD2011()[,-7] #retrait eduspec

regles = list(marital = "age < 18")
regles_val = list(marital = "SINGLE")

visit_sequence = c(5,2,21,22,23,15,10,1,17,18,16,14,20,11,12,13,19,6,3,8,4,9,7)
names(df)[visit_sequence]

# Initialisation
mes_modeles <- c("cart","ctree","parametric", "rf")

n_sim <- 100
num_seed <- 40889

# Simulations (cart, ctree, rf, parametric)
res_simulation <- list(
  meta = as.list(rep("", length(mes_modeles))),
  data = as.list(rep("", length(mes_modeles)))
)
names(res_simulation$meta) <- mes_modeles
names(res_simulation$data) <- mes_modeles

plan(multisession, workers = length(mes_modeles))

list_calcul <- furrr::future_map(
  mes_modeles,
  \(meth){
    res <- syn(
      df, method = meth, m = n_sim, 
      visit.sequence = visit_sequence, 
      rules = regles, rvalues = regles_val, 
      models = TRUE, seed = num_seed
    )
    res$models <- res$models[[1]]
    return(res)
  },
  .options = furrr_options(seed = TRUE)
)
names(list_calcul) <- mes_modeles

plan(sequential)

res_simulation$meta <- list_calcul %>% 
  purrr::map(  \(calcul){
    res <- calcul[-3]
    return(res)
  })

res_simulation$data <- list_calcul %>% 
  purrr::map(  \(calcul) calcul$syn )

## Sauvegarde 

BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

# Les fichiers du bucket 
aws.s3::get_bucket(BUCKET, region = "")
aws.s3::put_bucket(BUCKET_SIM, region = "")

date = format(Sys.Date(), "%Y%m%d")
FILE_KEY_OUT_S3 = paste0(
  date, 
  "_sim_synthpop_", 
  paste0(mes_modeles, collapse = "_"),
  "_",
  n_sim, 
  "_sims.RDS"
)

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