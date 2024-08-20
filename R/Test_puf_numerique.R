if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
FILE_KEY_IN_S3 = "puf_preprocessed.RDS"
puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)

# Test sur les 8 NAF -----------------------------------------------------------
df <- puf[, c("NAFANTG004N", "NAFANTG088N", "NAFG004UN", "NAFG010UN", "NAFG017UN",
              "NAFG021UN", "NAFG038UN", "NAFG088UN")]

df <- df %>%
  mutate_if(is.factor, as.numeric)

tic()
synth <- syn(df, seed = 1)  
toc()

compare(synth, df)
pMSE_df <- utility.gen(synth, df, nperms = 1)$pMSE  # 8.903371e-06


table(puf$NAFANTG088N, synth$syn$NAFANTG088N) 
# On remarque des effectifs assez similaires

# Test sur toutes les variables-------------------------------------------------
df1 <- puf

df1 <- df1 %>%
  mutate_if(is.factor, as.numeric)

tic()
syn1 <- syn(df1, minnumlevels = 1, seed = 1)  # 603.223 sec
toc()

pMSE_df1 <- utility.gen(syn1, df1, nperms = 1)$pMSE  # 0.0002347067

# Comparaison à puf 65 variables non encodé en numérique -----------------------
# Pour puf65 on obtient un pMSE de 0.11

df2 <- puf[, -c("ISCO2", "NAFANTG088N", "NAFG038UN", "NAFG088UN", "PCS2")]
syn2 <- syn(df2, minnumlevels = 1, seed = 1)

pMSE_df2 <- utility.gen(syn2, df2, nperms = 1)




# Tests ------------------------------------------------------------------------





