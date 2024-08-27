if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)

# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

df_tvae <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "df1_tvae_best.csv",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)

df_ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "df1_ctgan_best.csv",
  bucket = file.path(BUCKET, "simulations"),
  opts = list("region" = "")
)


# Jeux de donnés ---------------------------------------------------------------
df <- data$original

sexe_levels <- levels(df$sex)
edu_levels <- levels(df$edu)

df_cart <- data$cart[[1]]
df_cart$sex <- factor(df_cart$sex, levels =sexe_levels)
df_cart$edu <- factor(df_cart$edu, levels =edu_levels)

df_tvae <- df_tvae[, 2:23] %>%
  # select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol")) %>%
  mutate_if(is.character, as.factor)

df_tvae$sex <- factor(df_tvae$sex, levels =sexe_levels)
df_tvae$edu <- factor(df_tvae$edu, levels =edu_levels)

df_tvae <- as.data.frame(df_tvae)

df_ctgan <- df_ctgan[, 2:23] %>%
  # select(c("sex", "age", "placesize", "edu", "socprof", "ls", "alcsol")) %>%
  mutate_if(is.character, as.factor) 

df_ctgan <- as.data.frame(df_ctgan)

df_ctgan$sex <- factor(df_ctgan$sex, levels =sexe_levels)
df_ctgan$edu <- factor(df_ctgan$edu, levels =edu_levels)


# Modèle original -----------

get_confint <- function(data, nom){
  
  model_original <- lm(weight ~ height + age + edu, data = data)
  
  ci <- confint(model_original, level = 0.95)
  
  ci <- ci %>% 
    as_tibble() %>%
    mutate(vars = rownames(ci), modele = nom) %>% 
    slice(-1) %>% 
    select(4:3,1:2)
  
  names(ci) <- c("modele", "vars","low","up")
  
  return(ci)
}

cio_function <- function(lo, uo, ls, us){
  min_u <- pmin(uo, us)
  max_l <- pmax(lo,ls)
  1/2*((min_u - max_l)/(uo-lo) + (min_u - max_l)/(us-ls))
}

all_confints <- purrr::imap(
  list("1-original" = df, "2-cart" = df_cart, "3-ctgan" = df_ctgan, "4-tvae" = df_tvae),
  get_confint
)

all_cios <- all_confints[1:4] |>
  purrr::list_rbind() |>
  full_join(all_confints[["1-original"]] %>%  select(-1) %>% rename(low_orig = low, up_orig = up)) |>
  mutate(cio = cio_function(low_orig, up_orig, low, up)) |>
  arrange(vars, modele) %>% 
  group_by(modele) %>% 
  mutate(num = 1:n()) %>% 
  group_by(vars) %>% 
  mutate(num = num + c(-0.15,-0.05,0.05,0.15))
# mutate(modele = factor(modele, levels = c("original","cart","ctgan","tvae"), ordered = TRUE)) %>% 


all_cios %>% 
  ggplot() +
  geom_segment( aes(x=num, xend=num, y=low, yend=up, color = modele)) +
  geom_point( aes(x=num, y=low, color = modele), size=1.5 ) +
  geom_point( aes(x=num, y=up, color = modele), size=1.5 ) +
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey25") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(breaks = 1:5, labels = sort(unique(all_confints$`1-original`$vars))) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal(base_size = 20) +
  guides(color = guide_legend("Modèle")) +
  theme(
    axis.line = element_line(linewidth = 0.45, colour = "grey5"),
    legend.position = "inside", 
    legend.position.inside = c(0.85,0.2), legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5)
  ) +
  xlab("") +
  ylab("Coefficient de régression")

ggsave(filename = "sd2011_intervalle_confiance_lm.pdf", device = "pdf", width = 12, height = 6)


resume_original <- summary(lm(weight ~ height + age + edu, data = df))$coefficients[,c(1,4)] 

all_cios %>% 
  ungroup() %>% 
  filter(modele != "1-original") %>% 
  select(modele, vars, cio) %>% 
  tidyr::pivot_wider(names_from = modele, values_from = cio) %>% 
  full_join(resume_original %>% 
              as_tibble() %>% 
              mutate(vars = rownames(resume_original)) %>% 
              slice(-1)) %>% 
  select(1, 5,6,2:4) %>% 
  knitr::kable(
    format = "latex", digits = 3, booktabs = TRUE
  )

# # Synthétisations --------------------------------------------------------------
# syn_cart <- syn(df_cart,
#                 seed = 1)
# 
# syn_ctgan <- syn(df_ctgan,
#                  seed = 1)
# syn_ctgan$syn <- df_ctgan
# 
# syn_tvae <- syn(df_tvae,
#                 seed = 1)
# syn_tvae$syn <- df_tvae
# 
# # CIO --------------------------------------------------------------------------
# model.ods <- glm(alcsol ~ ., data = df, family = binomial)
# model.sds_cart <- glm.synds(alcsol ~ ., data = syn_cart, family = binomial)
# model.sds_ctgan <- glm.synds(alcsol ~ ., data = syn_ctgan, family = binomial)
# model.sds_tvae <- glm.synds(alcsol ~ ., data = syn_tvae, family = binomial)
# 
# compare(model.sds_cart, df)
# compare(model.sds_ctgan, df)
# compare(model.sds_tvae, df)

