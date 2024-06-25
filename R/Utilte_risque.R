if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
source("~/work/synthetic-data-sdc/R/fonctions/Utilite.R")

# Import des données -----------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")
FILE_KEY_IN_S3_2 <- "resultats_analyses.RDS"

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET_SIM_1,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

analyses <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_2,
  bucket = BUCKET_SIM_2,
  opts = list("region" = "")
)
names(analyses) <- c("data_empile", "data_original", "data_empile_combined", "table_mean_num",
                     "table_org_mean_num", "table_sd_num", "table_cat", "table_cor",
                     "cor_comp", "somme_cor_mat", "table_mae", "table_mse", "reg_coeff",
                     "utility_measures_all_meth", "nb_repliques_all_meth", "KS_test")

# Table Risque/Utilite ---------------------------------------------------------
table_ru <- matrix(0, nrow = 2, ncol = length(mes_modeles))
colnames(table_ru) <- mes_modeles
row.names(table_ru) <- c("Risque (TCAP moyen)", "Utilité (pMSE moyen)")
for (i in 1:length(mes_modeles)) {
  tcap_modele <- rep(0, nrow = length(data[[i]]))
  for (j in 1:length(data[[i]])) {
    tcap_modele[j] <- calc_TCAP(data$original, data[[i]][[j]], c("sex", "age"), "depress")
  }
  table_ru[1, i] <- mean(tcap_modele, na.rm = T)
}

table_ru[2, 1] <- mean(analyses$utility_measures_all_meth$pMSE[1:500])
table_ru[2, 2] <- mean(analyses$utility_measures_all_meth$pMSE[501:1000])
table_ru[2, 3] <- mean(analyses$utility_measures_all_meth$pMSE[1001:1500])
table_ru[2, 4] <- mean(analyses$utility_measures_all_meth$pMSE[1501:2000])
table_ru[2, 5] <- mean(analyses$utility_measures_all_meth$pMSE[2001:2500])
table_ru[2, 6] <- mean(analyses$utility_measures_all_meth$pMSE[2501:3000])


# Graphiques -------------------------------------------------------------------
data_plot <- data.frame(
  Risque_TCAP = table_ru[1, ],
  Utilite_pMSE = table_ru[2, ],
  Modeles = mes_modeles
)

# Tracer le graphique
ggplot(data_plot, aes(x = Risque_TCAP, y = Utilite_pMSE)) +
  geom_point() +
  geom_text(aes(label = Modeles), vjust = -1) +
  labs(x = "Risque (TCAP moyen)", y = "Utilité (pMSE moyen)") +
  theme_minimal()



