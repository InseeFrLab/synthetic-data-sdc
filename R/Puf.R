if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("rcompanion", quietly = TRUE)) install.packages("rcompanion"); library(rcompanion)
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart"); library(rpart)
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot"); library(rpart.plot)
source("~/work/synthetic-data-sdc/R/fonctions/Correlations_mixtes.R")

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
puf.afdm <- FAMD(puf_test, ncp = 1000, graph = FALSE)
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
                visit.sequence = classement_var(puf_test, puf.afdm)[[2]],
                maxfaclevels = 100,
                cont.na = list(HEFFEMP = -8,
                               HEFFTOT = -8,
                               HHABEMP = -8,
                               HHABTOT = -8),
                seed = 1)
toc()
# 1331 sec 

tic()
pMSE_afdm <- utility.gen(syn_afdm, puf_test, nperms = 1)$pMSE
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

tic()
syn_test <- syn(puf_test,
                maxfaclevels = 100,
                seed = 1)
toc()
# 1465 sec

tic()
syn_ctree <- syn(puf_test,
                 maxfaclevels = 100,
                 method = "ctree",
                 seed = 1)
toc()


pMSE_ctree <- utility.gen(syn_ctree, puf_test, nperms = 1)$pMSE

utility.gen(puf_syn, puf_test, nperms = 1)$pMSE # 0.1914136

pMSE_cart <- utility.gen(syn_test, puf_test, nperms = 1)$pMSE # 0.0008927075

write.csv(puf_test, "~/work/synthetic-data-sdc/TableEvaluator/puf.csv")


for (i in 1:length(puf)) {
  #cat(names(puf)[i], " : ", sum(is.na(puf[[names(puf)[i]]])), "\n")
  cat(names(puf)[i], " : ", sum(puf[[names(puf)[i]]] == ""), "\n")
}

# Croisements ------------------------------------------------------------------
croisements <- function(dataorg, datasyn, variable1, variable2) {
  liste <- list()
  liste[[1]] <- table(dataorg[[variable1]], dataorg[[variable2]], useNA = "always")
  liste[[2]] <- table(datasyn[[variable1]], datasyn[[variable2]], useNA = "always")
  names(liste) <- c("Original", "Synthétique")
  return(liste)
}

# Graphiques -------------------------------------------------------------------

liste_graph <- list()

for (i in 1:length(puf_test)) {
  freq_puf_test <- puf_test %>%
    count(.data[[names(puf_test)[i]]]) %>%
    rename(effectif = n) %>%
    mutate(dataset = "Original")
  
  freq_syn_puf <- syn_test$syn %>%
    count(.data[[names(syn_test$syn)[i]]]) %>%
    rename(effectif = n) %>%
    mutate(dataset = "Synthétique")
  
  freq_combined <- bind_rows(freq_puf_test, freq_syn_puf)
  
  plt <- ggplot(freq_combined, aes(x = .data[[names(puf_test)[i]]], y = effectif, fill = dataset)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = paste("Comparaison des effectifs de", names(puf_test)[i]),
         x = names(puf_test)[i],
         y = "Effectif",
         fill = "Jeu de données") +
    scale_fill_manual(values = c("Original" = "#1A3C5A", "Synthétique" = "#4187BF")) +
    theme_minimal()
  
  liste_graph[[i]] <- plt
}
print(liste_graph)

# Corrélations
mixed_correlations(puf_test)



# Répliqués --------------------------------------------------------------------
replicated.uniques(syn_test, puf_test)

duplicated(puf_test)
duplicated(syn_test$syn)

df_comb <- rbind(puf_test, syn_test$syn)
duplicated(df_comb)

# Rpart ------------------------------------------------------------------------
df <- cbind(puf_test, puf[, "NAFG038UN"])
fit <- rpart(NAFG038UN ~ ., data = df)

rpart.plot(fit)


naf38_pred <- predict(fit, df, type = "class")

