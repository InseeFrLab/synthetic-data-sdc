if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("rcompanion", quietly = TRUE)) install.packages("rcompanion"); library(rcompanion)

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
puf.afdm <- FAMD(puf, ncp = 1000, graph = FALSE)
puf.afdm$eig[, 3]


classement_var <- function(data, data.afdm) {
  eig.val <- data.afdm$eig
  matrice <- matrix(0, nrow = 1, ncol = length(data))
  for (i in 1:length(data)) {
    var <- 0
    for (j in 1:eig.val[, 2]) {
      var <- var + eig.val[, 2][j] * data.afdm$var$contrib[names(data)[i], j]
    }
    matrice[1, i] <- var
  }
  colnames(matrice) <- names(data)
  matrice <- matrice[, order(matrice, decreasing = TRUE)]
  ordre <- names(matrice)
  return(list(matrice, ordre))
}

varexp <- function(data.afdm) {
  barplot(data.afdm$eig[, 2], 
          names.arg = 1:nrow(data.afdm$eig), 
          main = "Variance expliquée par dimensions (%)",
          xlab = "Principales dimensions",
          ylab = "Pourcentage de variance",
          col ="steelblue")
  lines(x = 1:nrow(data.afdm$eig), data.afdm$eig[, 2], 
        type = "b", pch = 19, col = "red")
  return(barplot)
}

tic()
syn_afdm <- syn(puf_test,
                    visit.sequence = classement_var(puf_test, puf_test.afdm)[[2]],
                    maxfaclevels = 100,
                    cont.na = list(HEFFEMP = -8,
                                   HEFFTOT = -8,
                                   HHABEMP = -8,
                                   HHABTOT = -8),
                    seed = 1)
toc()


tic()
pMSE_afdm <- utility.gen(syn_afdm, puf_test)$pMSE
toc()

# Tests ------------------------------------------------------------------------
puf_test <- puf[, -c("ISCO2", "NAFANTG088N", "NAFG038UN", "NAFG088UN", "PCS2")]
num_test <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac_test <- setdiff(names(puf_test), num_test)
vs_num_fac <- c(num_test, fac_test)

puf_test.afdm <- FAMD(puf_test, ncp = 400, graph = FALSE)

tic()
syn_ini <- syn(puf[, 23:32],
               seed = 1)
toc()

pm <- syn_ini$predictor.matrix

# 16 premières variables

tic()
syn1 <- syn(puf_test,
            visit.sequence = names(puf_test)[1:16],
            maxfaclevels = 100,
            drop.not.used = FALSE,
            seed = 1)
toc()

# 16 suivantes
tic()
syn2 <- syn(puf_test,
            visit.sequence = names(puf_test)[17:32],
            maxfaclevels = 100,
            cont.na = list(HEFFEMP = -8,
                           HEFFTOT = -8,
                           HHABEMP = -8,
                           HHABTOT = -8),
            drop.not.used = FALSE,
            seed = 1)
toc()

# 16 suivantes
tic()
syn3 <- syn(puf_test,
            visit.sequence = names(puf_test)[33:48],
            maxfaclevels = 100,
            drop.not.used = FALSE,
            seed = 1)
toc()

# 17 dernières variables
tic()
syn4 <- syn(puf_test,
            visit.sequence = names(puf_test)[49:65],
            maxfaclevels = 100,
            drop.not.used = FALSE,
            seed = 1)
toc()
###
puf_syn <- puf_test
puf_syn[, 1:16] <- syn1$syn[, 1:16]
puf_syn[, 17:32] <- syn2$syn[, 17:32]
puf_syn[, 33:48] <- syn3$syn[, 33:48]
puf_syn[, 49:65] <- syn4$syn[, 49:65]
###

utility.gen(puf_syn, puf_test, nperms = 1)$pMSE # 0.1914136

utility.gen(puf_syn, puf_test, nperms = 1)$pMSE # 0.1914136




for (i in 1:length(puf)) {
  #cat(names(puf)[i], " : ", sum(is.na(puf[[names(puf)[i]]])), "\n")
  cat(names(puf)[i], " : ", sum(puf[[names(puf)[i]]] == ""), "\n")
}



