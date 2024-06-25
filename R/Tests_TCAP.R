if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr"); library(purrr)
if (!requireNamespace("furrr", quietly = TRUE)) install.packages("furrr"); library(furrr)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse"); library(tidyverse)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis"); library(viridis)
if (!requireNamespace("abind", quietly = TRUE)) install.packages("abind"); library(abind)
if (!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly"); library(plotly)
if (!requireNamespace("wesanderson", quietly = TRUE)) install.packages("wesanderson"); library(wesanderson)
if (!requireNamespace("pracma", quietly = TRUE)) install.packages("pracma"); library(pracma)
if (!requireNamespace("hrbrthemes", quietly = TRUE)) install.packages("hrbrthemes"); library(hrbrthemes)
if (!requireNamespace("rmarkdown", quietly = TRUE)) install.packages("rmarkdown"); library(rmarkdown)
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr"); library(knitr)
if (!requireNamespace("rcompanion", quietly = TRUE)) install.packages("rcompanion"); library(rcompanion)
source("R/fonctions/TCAP.R")

# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")


var_quasi_id <- c("sex", "age", "agegr", "placesize", "edu", "socprof", "marital")
var_sensi <- c("depress", "trust", "trustfam", "trustneigh", "ls", "sport", "smoke", "alcabuse", "alcsol")


tictoc::tic()
WEAP1 <- calc_WEAP(data$cart[[1]], var_quasi_id, var_sensi)
tictoc::toc()
#  sec elapsed

data_syn_filtered1 <- filter_WEAP_1(data$cart[[1]], WEAP1)

tictoc::tic()
TCAP1 <- calc_TCAP(data$original, data_syn_filtered1, var_quasi_id, var_sensi)
tictoc::toc()
#  sec elapsed

print(TCAP1)
print(mean(TCAP1, na.rm = TRUE))