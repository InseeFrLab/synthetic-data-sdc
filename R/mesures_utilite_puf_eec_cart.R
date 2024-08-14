if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2"); library(ggplot2)
if (!requireNamespace("rpart", quietly = TRUE)) install.packages("rpart"); library(rpart)
if (!requireNamespace("rpart.plot", quietly = TRUE)) install.packages("rpart.plot"); library(rpart.plot)

source("R/fonctions/classement_var_par_afdm.R")

set.seed(40889)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"
FILE_KEY_IN_S3 = "puf_preprocessed.RDS"
puf <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET,
  opts = list("region" = "")
)

str(puf)

# Selection des variables ------

puf_num <- puf |> select(-EXTRIAN, -NOI) |> mutate(across(everything(), as.numeric))

correspondance_cat_num <- purrr::map(
  puf |> select(-EXTRIAN),
  \(v) tibble(levels = levels(v), num = sort(as.numeric(unique(v))))
)
names(correspondance_cat_num) <- puf |> select(-EXTRIAN) |> colnames()

str(puf_num)
summary(puf_num)

# Synthétisation avec synthpop -------------

visite_seq <- classement_var(puf_num, all_num = TRUE)$ordre
# visite_seq2 <- classement_var(puf, all_num = FALSE)$ordre

puf_cart <- puf_num |> syn(method = "cart", seed = 40889)
puf_cart_vs <- puf_num |> syn(method = "cart", visit_sequence = visite_seq, seed = 40889)

# Mesure du pMSE avec synthpop

utility_synthpop_puf_cart <- utility.gen(puf_cart, puf_num)
utility_synthpop_puf_cart_vs <- utility.gen(puf_cart_vs, puf_num)


# Mesure personnelle du PMSE

pMSE <- function(orig, synth){
  all_data <- bind_rows(orig |> mutate(synth = 0), synth |> mutate(synth = 1))
  fit_synth <- rpart(synth ~ ., data = all_data, method = "class")
  propensity_score <- predict(fit_synth, all_data, type = "class")
  
}





