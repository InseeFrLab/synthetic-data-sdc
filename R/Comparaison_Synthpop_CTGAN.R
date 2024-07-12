if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)

source("R/fonctions/Scores_propension.R")

# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")
FILE_KEY_IN_S3_2 <- "resultats_analyses.RDS"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")
FILE_KEY_IN_S3_3 <- "simulations/ctgan_simulation_13_06_2024.csv"

aws.s3::get_bucket(BUCKET, region = "")

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = FILE_KEY_IN_S3_3,
  bucket = BUCKET,
  opts = list("region" = "")
)
ctgan <- as.data.frame(ctgan) # car ctgan est un tibble
ctgan <- ctgan[, 2:23]
#ctgan[fac] <- factor(ctgan[, fac])

num = c("age", "depress", "nofriend", "height", "weight", "bmi")
fac = c("sex", "agegr", "placesize", "edu", "socprof", "marital", "ls", "trust",
        "trustfam", "trustneigh", "sport", "smoke", "alcabuse", "alcsol", "wkabint",
        "englang")

# Utilite ----------------------------------------------------------------------
#utilite <- utility.gen(data$original, ctgan)

score_propension(data$original, ctgan)$pMSE
score_propension(data$original, data$cart[[1]])$pMSE


# Tests ------------------------------------------------------------------------
dim(subset(data$cart[[1]], marital == "MARRIED" & age < 18))[1]
dim(subset(ctgan, marital == "MARRIED" & age < 18))[1]


tvae_mat_bs_1000 <- matrix(0, nrow=4, ncol=4)
colnames(tvae_mat_bs_1000) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")
row.names(tvae_mat_bs_1000) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")
tvae_mat_bs_500 <- matrix(0, nrow=4, ncol=4)
colnames(tvae_mat_bs_500) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")
row.names(tvae_mat_bs_500) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")
tvae_mat_bs_100 <- matrix(0, nrow=4, ncol=4)
colnames(tvae_mat_bs_100) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")
row.names(tvae_mat_bs_100) <- c("[64,64]", "[128,128]", "[256,256]", "[128,128,128]")

test_tvae <- read.csv("~/work/synthetic-data-sdc/bs_100_comp_dims_[128, 128, 128]_decomp_dims_[128, 128, 128].csv")
test_tvae <- test_tvae[,2:23]
pMSE_bs_100_cd_128_128_128_dd_128_128_128 <- utility.gen(test_tvae, data$original)$pMSE

tvae_mat_bs_100[4,4] <- pMSE_bs_100_cd_128_128_128_dd_128_128_128






liste_matrices_ctgan <- list(ctgan_mat_bs_100,
                       ctgan_mat_bs_500,
                       ctgan_mat_bs_1000)
names(liste_matrices_ctgan) <- c("ctgan_mat_bs_100",
                                 "ctgan_mat_bs_500",
                                 "ctgan_mat_bs_1000")

liste_matrices_tvae <- list(tvae_mat_bs_100,
                             tvae_mat_bs_500,
                             tvae_mat_bs_1000)
names(liste_matrices_tvae) <- c("tvae_mat_bs_100",
                                "tvae_mat_bs_500",
                                "tvae_mat_bs_1000")

FILE_KEY_OUT_S3 <- "pMSE_ctgan.RDS"
FILE_KEY_OUT_S3_tvae <- "pMSE_tvae.RDS"


aws.s3::s3write_using(
  liste_matrices_ctgan,
  FUN = saveRDS,
  object = FILE_KEY_OUT_S3,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)

aws.s3::s3write_using(
  liste_matrices_tvae,
  FUN = saveRDS,
  object = FILE_KEY_OUT_S3_tvae,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)





