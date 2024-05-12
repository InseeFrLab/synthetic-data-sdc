library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)

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

tictoc::tic()

utility_measures_all_meth <- imap(
  res_simul[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        synthpop::utility.gen(
          df_synth,
          res_simul$original, 
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

tictoc::toc()
