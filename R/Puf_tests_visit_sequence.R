if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)


# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

puf65 <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "puf_preprocessed.RDS",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf71 <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "puf.RDS",
  bucket = BUCKET,
  opts = list("region" = "")
)

# Création des jeux de données -------------------------------------------------
puf1 <- puf71[puf71$TRIM == 1,]
puf1 <- puf1[, !names(puf1) %in% c("IDENT", "NAFANTG088N", "NAFG088UN")]

num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac <- setdiff(names(puf1), num)


vect <- rep(0, length(names(puf1)))
for (i in 1:length(names(puf1))) {
  vect[i] <- length(unique(puf1[, i]))
  names(vect) <- names(puf1)
}







