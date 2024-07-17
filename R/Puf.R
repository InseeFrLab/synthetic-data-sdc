if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
FILE_KEY_IN_S3 = "puf_preprocessed.RDS"
data_puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)


# Préprocessing ----------------------------------------------------------------
char <- setdiff(names(puf), c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT"))

data_puf <- puf[, (char) := lapply(.SD, as.character), .SDcols = char] # On passe les variables nécessaire en character
data_puf <- data_puf[data_puf$TRIM == 1,] # Premier Trimestre
data_puf <- data_puf[,-c("ANNEE", "TRIM")] # On retire les variables ANNEE et TRIM

# AFDM -------------------------------------------------------------------------
res.afdm <- FAMD(data_puf)

# Synthétisation ---------------------------------------------------------------
data_puf <- data_puf[, -c("IDENT", "NAFANTG004N", "NAFG004UN", "NAFG010UN", 
                          "NAFG017UN", "NAFG021UN", "NAFG038UN", "PCS1", "PCS1Q")]

tic()
syn_puf <- syn(data_puf, maxfaclevels = 90, seed = 1)
toc()
pMSE_data_puf <- utility.gen(syn_puf, puf_test)$pMSE

# Tests ------------------------------------------------------------------------











