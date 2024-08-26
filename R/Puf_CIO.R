if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr"); library(dplyr)
if (!requireNamespace("synthpop", quietly = TRUE)) install.packages("synthpop"); library(synthpop)
if (!requireNamespace("tictoc", quietly = TRUE)) install.packages("tictoc"); library(tictoc)
if (!requireNamespace("aws.s3", quietly = TRUE)) install.packages("aws.s3"); library(aws.s3)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr"); library(tidyr)


# Importation ------------------------------------------------------------------
BUCKET = "projet-donnees-synthetiques"

num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
fac <- setdiff(names(puf65), num)
puf65 <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65 <- puf65[, 2:66]
# num <- c("EXTRIAN", "HEFFEMP", "HEFFTOT", "HHABEMP", "HHABTOT")
# fac <- setdiff(names(puf65), num)
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

puf65_ctgan <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_ctgan.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65_ctgan <- puf65_ctgan[, 2:66]
puf65_ctgan <- puf65_ctgan %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

puf65_tvae <- aws.s3::s3read_using(
  FUN = readr::read_csv,
  object = "puf65_tvae.csv",
  bucket = BUCKET,
  opts = list("region" = "")
)
puf65_tvae <- puf65_tvae[, 2:66]
puf65_tvae <- puf65_tvae %>%
  mutate_if(is.numeric, as.factor) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(across(all_of(num), as.numeric))

# Jeux de donnés ---------------------------------------------------------------
df <- puf65 %>%
  mutate(across(where(is.factor), as.character)) %>% 
  filter(ACTEU %in% 1:2 & AGE6 != "90") %>% 
  select(c("SEXE", "AGE6", "DIP7", "ACTEU", "METRODOM")) %>% 
  mutate(IS_CHOM = ACTEU == 2)

df_cart <- puf_cart %>%
  mutate(across(where(is.factor), as.character)) %>% 
  filter(ACTEU %in% 1:2 & AGE6 != "90") %>% 
  select(c("SEXE", "AGE6", "DIP7", "ACTEU", "METRODOM")) %>% 
  mutate(IS_CHOM = ACTEU == 2)
df_cart <- as.data.frame(df_cart)

df_tvae <- puf65_tvae %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  filter(ACTEU %in% 1:2 & AGE6 != "90") %>% 
  select(c("SEXE", "AGE6", "DIP7", "ACTEU", "METRODOM")) %>% 
  mutate(IS_CHOM = ACTEU == 2)
df_tvae <- as.data.frame(df_tvae)

df_ctgan <- puf65_ctgan %>%
  mutate(across(where(is.factor), as.character)) %>% 
  filter(ACTEU %in% 1:2 & AGE6 != "90") %>% 
  select(c("SEXE", "AGE6", "DIP7", "ACTEU", "METRODOM")) %>% 
  mutate(IS_CHOM = ACTEU == 2)
df_ctgan <- as.data.frame(df_ctgan)

# Modèles -----------------
get_confint_OR <- function(data, nom){
  
  model_original <- glm(IS_CHOM ~ SEXE + AGE6 + DIP7 + METRODOM, data = data)
  
  ci <- exp(confint(model_original, level = 0.95))
  
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
  get_confint_OR
)

all_cios <- all_confints[1:4] |>
  purrr::list_rbind() |>
  full_join(all_confints[["1-original"]] %>%  select(-1) %>% rename(low_orig = low, up_orig = up)) |>
  mutate(cio = cio_function(low_orig, up_orig, low, up)) |>
  arrange(vars, modele) %>% filter(!is.na(low_orig) & !is.na(up_orig)) %>% 
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
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey25") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_x_continuous(breaks = 1:12, labels = sort(unique(all_confints$`1-original`$vars))) +
  scale_y_continuous(breaks = seq(0.5,1.75,0.25), limits = c(0.5,1.75), expand = c(0,0)) +
  theme_minimal(base_size = 20) +
  guides(color = guide_legend("Modèle")) +
  theme(
    axis.line = element_line(linewidth = 0.45, colour = "grey5"),
    legend.position = c(0.85,0.2), legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5)
  ) +
  xlab("") +
  ylab("Odds Ratio")

ggsave(filename = "puf65_intervalle_confiance_glm.pdf", device = "pdf", width = 12, height = 6)


resume_original <- summary(glm(IS_CHOM ~ SEXE + AGE6 + DIP7 + METRODOM, data = df))$coefficients[,c(1,4)] 

all_cios %>% 
  ungroup() %>% 
  filter(modele != "1-original") %>% 
  select(modele, vars, cio) %>% 
  tidyr::pivot_wider(names_from = modele, values_from = cio) %>% 
  full_join(resume_original %>% 
              as_tibble() %>% 
              mutate(vars = rownames(resume_original)) %>% 
              slice(-1)) %>% 
  arrange(vars) %>% 
  select(1,6,2:4) %>% 
  knitr::kable(
    format = "latex", digits = 3, booktabs = TRUE
  )




# 
# 
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
# model.ods <- glm(ENFRED ~ ., data = df, family = binomial)
# model.sds_cart <- glm.synds(ENFRED ~ ., data = syn_cart, family = binomial)
# model.sds_ctgan <- glm.synds(ENFRED ~ ., data = syn_ctgan, family = binomial)
# model.sds_tvae <- glm.synds(ENFRED ~ ., data = syn_tvae, family = binomial)
# 
# compare(model.sds_cart, df)
# compare(model.sds_ctgan, df)
# compare(model.sds_tvae, df)
# 
