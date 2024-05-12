packages <- c("synthpop","dplyr","furrr","purrr","aws.s3")
p <- lapply(packages, \(pack) if(! pack %in% installed.packages()) install.packages(pack))

library(synthpop)
library(dplyr)
library(furrr)
library(purrr)
source("R/fonctions/creation_jeu.R")

# Jeu de données
df <- jeudedonnees_SD2011()

regles = list(marital = "age < 18")
regles_val = list(marital = "SINGLE")

visit_sequence = c(2,20,21,22,14,9,1,16,17,15,13,19,10,11,12,18,5,3,7,4,8,6)
names(df)[visit_sequence]

# Initialisation
mes_modeles <- c("sample","cart","ctree","parametric","bag","rf")

n_sim <- 500
num_seed <- 40889


tictoc::tic()

res_simulation <- map(
  mes_modeles,
  \(modele){
    print(modele)
    
    plan(multisession, workers = 25)
    
    res <- furrr::future_map(
      seq_len(n_sim), \(i){
        r <- syn(
          df, method = modele, m = 1, 
          visit.sequence = visit_sequence, 
          rules = regles, rvalues = regles_val, 
          models = TRUE
          # , seed = num_seed
        )
        return(r$syn)
      }, .options = furrr_options(seed = num_seed), 
      .progress = TRUE
    )
    plan(sequential)
    return(res)
  },
  .progress = TRUE
)
names(res_simulation) <- mes_modeles

tictoc::toc()
#3739.65 sec elapsed pour n_sim=100
#19937.91 sec elapsed pour n_sim = 500

res_simulation$original <- df
format(object.size(res_simulation), units = "Gb")

date = format(Sys.Date(), "%Y%m%d")
save(res_simulation, file = 
       file.path("X:/HAB-INVEST-CONFIDENTIALITE/SYNTHETIC/simulations",
                 paste0(
                   date, 
                   "_sim_synthpop_", 
                   paste0(mes_modeles, collapse = "_"),
                   "_",
                   n_sim, 
                   "_sims.RData"
                 )
       )
)

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