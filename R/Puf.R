if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
FILE_KEY_IN_S3 = "puf_preprocessed.RDS"
puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)


# AFDM -------------------------------------------------------------------------
puf.afdm <- FAMD(puf, ncp = 500, graph = FALSE)
puf.afdm$eig[, 3]

eig.val <- puf.afdm$eig[, 1]

classement_var <- function() {
  matrice <- matrix(0, nrow = 1, ncol = length(puf))
  for (i in 1:length(puf)) {
    var <- 0
    for (j in 1:eig.val[, "variance.percent"]) {
      var <- var + eig.val[, "variance.percent"][j] * puf.afdm$var$contrib[names(puf)[i], j]
    }
    var <- var / 100
    matrice[1, i] <- var
  }
  colnames(matrice) <- names(puf)
  matrice <- matrice[, order(matrice, decreasing = TRUE)]
  return(matrice)
}

varexp <- function() {
  barplot(eig.val[, 2], 
          names.arg = 1:nrow(eig.val), 
          main = "Variance expliquée par dimensions (%)",
          xlab = "Principales dimensions",
          ylab = "Pourcentage de variance",
          col ="steelblue")
  lines(x = 1:nrow(eig.val), eig.val[, 2], 
        type = "b", pch = 19, col = "red")
  print(barplot)
}

# Tests ------------------------------------------------------------------------
for (i in 1:length(puf)) {
  cat(names(puf)[i], " : ", sum(is.na(puf[[names(puf)[i]]])), "\n")
}

variables <- c("ACTEU", "AGE6", "COUPL_LOG", "NAFG004UN", "NAFG010UN", "NAFG017UN", "TYPLOG5")

puf_mod <- puf[, c("ACTEU", "AGE6", "COUPL_LOG", "NAFG004UN", "NAFG010UN", "NAFG017UN", "TYPLOG5")]


tic()
syn_puf <- syn(puf,
                    maxfaclevels = 100,
                    cont.na = list(HEFFEMP = -8,
                                   HEFFTOT = -8,
                                   HHABEMP = -8,
                                   HHABTOT = -8),
                    seed = 1)
toc()
pMSE_puf <- utility.gen(syn_puf, puf)$pMSE





















