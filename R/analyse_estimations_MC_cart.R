library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)

# Import des données -----------------
FILE_KEY_IN_S3 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

res_simul <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET_SIM,
  opts = list("region" = "")
)
str(res_simul, max.level=1)
methodes <- which(names(res_simul) != "original")

# Estimations MC ---------------------

indic_one_sim <- function(df){
  tibble(
    retired = df %>% count(socprof) %>% filter(socprof == "RETIRED") %>% pull(n),
    corr_taille_poids = cor(df$height, df$weight),
    female = df %>% count(sex) %>% filter(sex == "FEMALE") %>% pull(n),
    alcsol = df %>% count(alcsol) %>% filter(alcsol == "YES") %>% pull(n)
  )
}

indic_sims <- map(res_simul$cart[1:300], indic_one_sim) |> list_rbind()

valeurs_originales <- indic_one_sim(res_simul$original)

cummeans <- indic_sims |>
  mutate(across(everything(), cummean))

cumquant <- function(x, prob){
  
  map(1:length(x), \(i) quantile(x[1:i], probs = prob)) |> list_c()
  
}

cumq005 <- indic_sims |>
  mutate(across(everything(), ~cumquant(., prob = 0.05)))
cumq095 <- indic_sims |>
  mutate(across(everything(), ~cumquant(., prob = 0.95)))

valeurs_moyennes <- indic_sims |> summarise(across(everything(), mean))
valeurs_stds <- indic_sims |> summarise(across(everything(), sd))

biaisrel <-  (valeurs_moyennes / valeurs_originales - 1 ) *100
std <- valeurs_stds/valeurs_moyennes *100




