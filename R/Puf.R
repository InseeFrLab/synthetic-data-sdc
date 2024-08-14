if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("rcompanion", quietly = TRUE)) install.packages("rcompanion"); library(rcompanion)
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart"); library(rpart)
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot"); library(rpart.plot)
source("~/work/synthetic-data-sdc/R/fonctions/Correlations_mixtes.R")

# Preprocessing ----------------------------------------------------------------
puf <- puf %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate(across(all_of(num), as.numeric)) %>%
  mutate(across(all_of(fac), ~replace_na(as.character(.), "999"))) %>%
  mutate(across(all_of(num), ~replace_na(., -8))) %>%
  mutate_if(is.character, as.factor)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
FILE_KEY_IN_S3_1 = "puf_preprocessed.RDS"
puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_1,
  bucket = BUCKET,
  opts = list("region" = "")
)

FILE_KEY_IN_S3_2 = "puf.RDS"
puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_2,
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
#puf_test <- puf[, -c("ISCO2", "NAFANTG088N", "NAFG038UN", "NAFG088UN", "PCS2")]
puf_test <- cbind(puf[, 1:20], puf[, 22:33], puf[, 35:38], puf[, 41:46], puf[, 48:70])
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
tic()
syn_test <- syn(puf_test,
                maxfaclevels = 100,
                seed = 1)
toc()
# 1465 sec

# ISCO2 --------------------------------------------------------------------
df_isco <- cbind(puf_test[, 1:20],
                 puf[, "ISCO2"],
                 puf_test[, 21:65]
)
fit_isco <- rpart(ISCO2 ~ ., data = df_isco, method = "class")
isco_pred <- predict(fit_isco, df_isco, type = "class")

confusion_matrix <- table(isco_pred, df_isco$ISCO2)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

syn_puf_isco <- cbind(syn_test$syn[, 1:20],
                      isco_pred,
                      syn_test$syn[, 21:65])
names(syn_puf_isco)[21] = "ISCO2"

pMSE_puf_isco <- utility.gen(syn_puf_isco,
                             df_isco,
                             nperms = 1)$pMSE                                   # 0.11994
# ------------------------------------------------------------------------------

# NAFANTG088N ------------------------------------------------------------------
df_nafant88 <- cbind(puf_test[, 1:20],
                     puf[, "ISCO2"],
                     puf_test[, 21:33],
                     puf[, "NAFANTG088N"],
                     puf_test[, 34:65]
)
fit_nafant88 <- rpart(NAFANTG088N ~ ., data = df_nafant88, method = "class")

nafant88_pred <- predict(fit_nafant88, df_nafant88, type = "class")

confusion_matrix <- table(nafant88_pred, df_nafant88$NAFANTG088N)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

syn_puf_nafant88 <- cbind(syn_test$syn[, 1:20],
                          isco_pred,
                          syn_test$syn[, 21:33],
                          nafant_pred,
                          syn_test$syn[, 34:65]
)
names(syn_puf_nafant88)[21] = "ISCO2"
names(syn_puf_nafant88)[34] = "NAFANTG088N"

pMSE_puf_nafant88 <- utility.gen(syn_puf_nafant88,
                                 df_nafant88,
                                 nperms = 1)$pMSE                               # 
# ------------------------------------------------------------------------------

# NAFG038UN --------------------------------------------------------------------
df_naf38 <- cbind(puf_test[, 1:20],
                  puf[, "ISCO2"],
                  puf_test[, 21:33],
                  puf[, "NAFANTG088N"],
                  puf_test[, 34:38],
                  puf[, "NAFG038UN"],
                  puf_test[, 39:65]
)
fit_naf38 <- rpart(NAFG038UN ~ ., data = df_naf38, method = "class")

naf38_pred <- predict(fit_naf38, df_naf38, type = "class")

confusion_matrix <- table(naf38_pred, df_naf38$NAFG038UN)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

syn_puf_naf38 <- cbind(syn_test$syn[, 1:20],
                       isco_pred,
                       syn_test$syn[, 21:33],
                       nafant_pred,
                       syn_test$syn[, 34:38],
                       naf38_pred,
                       syn_test$syn[, 39:65]
)
names(syn_puf_naf38)[21] = "ISCO2"
names(syn_puf_naf38)[34] = "NAFANTG088N"
names(syn_puf_naf38)[39] = "NAFG038UN"

pMSE_puf_naf38 <- utility.gen(syn_puf_naf38,
                              df_naf38,
                              nperms = 1)$pMSE                               # 
# ------------------------------------------------------------------------------

# NAFG088UN --------------------------------------------------------------------
df_naf88 <- cbind(puf_test[, 1:20],
                  puf[, "ISCO2"],
                  puf_test[, 21:33],
                  puf[, "NAFANTG088N"],
                  puf_test[, 34:38],
                  puf[, "NAFG038UN"],
                  puf[, "NAFG088UN"],
                  puf_test[, 39:65]
)
fit_naf88 <- rpart(NAFG088UN ~ ., data = df_naf88, method = "class")

naf88_pred <- predict(fit_naf88, df_naf88, type = "class")

confusion_matrix <- table(naf88_pred, df_naf88$NAFG088UN)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

syn_puf_naf88 <- cbind(syn_test$syn[, 1:20],
                       isco_pred,
                       syn_test$syn[, 21:33],
                       nafant_pred,
                       syn_test$syn[, 34:38],
                       naf38_pred,
                       naf88_pred,
                       syn_test$syn[, 39:65]
)
names(syn_puf_naf88)[21] = "ISCO2"
names(syn_puf_naf88)[34] = "NAFANTG088N"
names(syn_puf_naf88)[39] = "NAFG038UN"
names(syn_puf_naf88)[40] = "NAFG088UN"

pMSE_puf_naf88 <- utility.gen(syn_puf_naf88,
                              df_naf88,
                              nperms = 1)$pMSE  
# ------------------------------------------------------------------------------

# PCS2 -------------------------------------------------------------------------
df_pcs <- cbind(puf_test[, 1:20],
                puf[, "ISCO2"],
                puf_test[, 21:33],
                puf[, "NAFANTG088N"],
                puf_test[, 34:38],
                puf[, "NAFG038UN"],
                puf[, "NAFG088UN"],
                puf_test[, 39:46],
                puf[, "PCS2"],
                puf_test[, 47:65]
)
fit_pcs <- rpart(PCS2 ~ ., data = df_pcs, method = "class")

pcs_pred <- predict(fit_pcs, df_pcs, type = "class")

confusion_matrix <- table(pcs_pred, df_pcs$PCS2)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

syn_puf_pcs <- cbind(syn_test$syn[, 1:20],
                     isco_pred,
                     syn_test$syn[, 21:33],
                     nafant_pred,
                     syn_test$syn[, 34:38],
                     naf38_pred,
                     naf88_pred,
                     syn_test$syn[, 39:46],
                     pcs_pred,
                     syn_test$syn[, 47:65]
)
names(syn_puf_pcs)[21] = "ISCO2"
names(syn_puf_pcs)[34] = "NAFANTG088N"
names(syn_puf_pcs)[39] = "NAFG038UN"
names(syn_puf_pcs)[40] = "NAFG088UN"
names(syn_puf_pcs)[47] = "PCS2"

pMSE_puf_pcs <- utility.gen(syn_puf_pcs,
                            df_pcs,
                            nperms = 1)$pMSE  
# ------------------------------------------------------------------------------




