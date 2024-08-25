if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR"); library(FactoMineR)
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra"); library(factoextra)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

puf65 <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf65 <- puf65[, 2:66]

puf65 <- puf65 %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

puf_cart <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf_cart.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)

puf_cart <- puf_cart[, 2:66]

puf_cart <- puf_cart %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))


df <- puf65 %>%
  select(c("AGE6", "ACTEU", "COUPL_LOG", "DIP7", "ENFRED", "SEXE", "TYPLOG5"))

df_cart <- puf_cart %>%
  select(c("AGE6", "ACTEU", "COUPL_LOG", "DIP7", "ENFRED", "SEXE", "TYPLOG5"))

# Fonctions --------------------------------------------------------------------
classement_var <- function(data, data.afdm) {
  eig.val <- data.afdm$eig
  matrice <- matrix(0, nrow = 1, ncol = length(data))
  for (i in 1:length(data)) {
    var <- 0
    for (j in 1:eig.val[, 2]) {
      var <- var + eig.val[, 2][j] * data.afdm$var$contrib[names(data)[i], j] / 100
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

# AFDM -------------------------------------------------------------------------
# PUF65
tictoc::tic()
syn_df <- syn(df,
              maxfaclevels = 100,
              seed = 1)
tictoc::toc()

tictoc::tic()
pMSE_df <- utility.gen(syn_df, df, nperms = 1)$pMSE
tictoc::toc()

# AFDM

df.afdm <- FAMD(df, ncp = 1000, graph = FALSE)

vs_df_afdm <- classement_var(df, df.afdm)[[2]]

tictoc::tic()
syn_df_afdm <- syn(df,
                visit.sequence = vs_df_afdm,
                maxfaclevels = 100,
                seed = 1)
tictoc::toc()

df_syn_afdm <- syn_df_afdm$syn
df <- as.data.frame(df)

tictoc::tic()
pMSE_df_afdm <- utility.gen(df_syn_afdm, df, nperms = 1)$pMSE
tictoc::toc()

# Puf_cart
df_cart.afdm <- FAMD(df_cart, ncp = 1000, graph = FALSE)

tictoc::tic()
syn_df_cart_afdm <- syn(df_cart,
                   visit.sequence = classement_var(df, df_cart.afdm)[[2]],
                   maxfaclevels = 100,
                   seed = 1)
tictoc::toc()

df_syn_cart_afdm <- syn_df_cart_afdm$syn
df_cart <- as.data.frame(df_cart) 

tictoc::tic()
pMSE_df_cart_afdm <- utility.gen(syn_df_cart_afdm, df_cart, nperms = 1)$pMSE
tictoc::toc()




