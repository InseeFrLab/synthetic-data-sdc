if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel"); library(ggrepel)
source("~/work/synthetic-data-sdc/R/fonctions/Utilite.R")
source("R/fonctions/Scores_propension.R")

# Import des données -----------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM_1 = file.path(BUCKET, "simulations")
FILE_KEY_IN_S3_1 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET_SIM_2 = file.path(BUCKET, "analyses")
FILE_KEY_IN_S3_2 <- "resultats_analyses.RDS"
FILE_KEY_IN_S3_3 <- "simulations/ctgan_simulation_13_06_2024.csv"

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

ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = FILE_KEY_IN_S3_3,
  bucket = BUCKET,
  opts = list("region" = "")
)
ctgan <- as.data.frame(ctgan) # car ctgan est un tibble
ctgan <- ctgan[, 2:23]

# Table Risque/Utilite ---------------------------------------------------------
table_ru <- matrix(0, nrow = 2, ncol = length(mes_modeles)+1)
colnames(table_ru) <- c(mes_modeles, "ctgan")
row.names(table_ru) <- c("Risque (TCAP moyen)", "Utilité (pMSE moyen)")
for (i in 1:length(mes_modeles)) {
  tcap_modele <- rep(0, nrow = length(data[[i]]))
  for (j in 1:length(data[[i]])) {
    tcap_modele[j] <- calc_TCAP(data$original, data[[i]][[j]], c("sex", "age"), "depress")
  }
  tcap_modele <- as.numeric(tcap_modele[tcap_modele != "TCAP indéfini"])
  table_ru[1, i] <- mean(tcap_modele, na.rm = T)
}

table_ru[2, 1] <- mean(analyses$utility_measures_all_meth$pMSE[1:500])
table_ru[2, 2] <- mean(analyses$utility_measures_all_meth$pMSE[501:1000])
table_ru[2, 3] <- mean(analyses$utility_measures_all_meth$pMSE[1001:1500])
table_ru[2, 4] <- mean(analyses$utility_measures_all_meth$pMSE[1501:2000])
table_ru[2, 5] <- mean(analyses$utility_measures_all_meth$pMSE[2001:2500])
table_ru[2, 6] <- mean(analyses$utility_measures_all_meth$pMSE[2501:3000])

# CTGAN
table_ru[1, 7] <- calc_TCAP(data$original, ctgan, c("sex", "age"), "depress")
table_ru[2, 7] <- score_propension(data$original, ctgan)$pMSE

# Graphiques -------------------------------------------------------------------
data_plot <- data.frame(
  Risque_TCAP = table_ru[1, ],
  Utilite_pMSE = table_ru[2, ],
  Modeles = c(mes_modeles, "ctgan")
)

# Tracer le graphique
ggplot(data_plot, aes(x = Utilite_pMSE, y = Risque_TCAP)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = Modeles), max.overlaps = 10, size = 10) +
  labs(x = "Utilité (pMSE moyen)", y = "Risque (TCAP moyen)") +
  scale_x_continuous(limits = c(0, 0.22), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0)) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))

 
