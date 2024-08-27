library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)


source("R/fonctions/creation_jeu.R")
# Jeu de données


# Import des données -----------------

methodes <- c("cart","sample","ctree","parametric","rf","ctgan_best","tvae_best")
heads <- c(rep(F, 5), T,T)

get_csv_SD2011_on_minio <- function(methode,h){
  
  df <- jeudedonnees_SD2011()
  
  FILE_KEY_IN_S3 <- paste0("df1_", methode, ".csv")
  BUCKET = "projet-donnees-synthetiques"
  BUCKET_SIM = file.path(BUCKET, "simulations")
  
  dat <- aws.s3::s3read_using(
    FUN = read.csv,
    header = h,
    object = FILE_KEY_IN_S3,
    bucket = BUCKET_SIM,
    opts = list("region" = "")
  )
  if(names(dat)[1] == "X" ) dat <- select(dat, -X)
  names(dat) <- names(df)
  return(dat)
}

all_df1_fcs <- map2(methodes, heads, get_csv_SD2011_on_minio)
names(all_df1_fcs) <- methodes

str(all_df1_fcs)


# Utilité globale ----------------
utility_global <- synthpop::utility.gen(all_df1_fcs, df, print.stats = "all")

# Utilité sur un tableau ------------

tableau1 <- c("sex","agegr","edu","placesize","smoke")
utility_tableau1 <- synthpop::utility.tab(all_df1_fcs, df, vars = tableau1, print.stats = "all")
synthpop::utility.tab(all_df1_fcs$cart, df, vars = tableau1, print.stats = "all")

dist_bhatt <- function(d_syn, name = "", d_orig){
  
  p_q <- d_orig %>% 
    count(across(all_of(tableau1))) %>% 
    mutate(p_orig = n/sum(n)) %>%
    rename(n_orig = n) %>% 
    full_join(
      d_syn %>% 
        count(across(all_of(tableau1))) %>% 
        mutate(p_syn = n/sum(n)) %>% 
        rename(n_syn = n)
    ) %>% 
    mutate(across(c(p_orig, p_syn), ~ifelse(is.na(.), 0, .)))
  
  bhatt = -log(sum(sqrt(p_q$p_orig*p_q$p_syn)))
  hell = philentropy::hellinger(p_q$p_orig, p_q$p_syn, testNA = FALSE)
  
  #donne le meme resultat que:
  #philentropy::bhattacharyya(p_q$p_orig, p_q$p_syn, testNA = FALSE, unit = "log", epsilon = 1e-5)
  return(tibble(name = name, bhatt = bhatt, hell = hell))
}

d_orig = jeudedonnees_SD2011()
all_bhatt <- imap(all_df1_fcs, dist_bhatt, d_orig = d_orig) |> list_rbind()


# Utilité sur les marges ------------

dist_marges <- function(d_syn, name = "", d_orig, cat_vars){

  purrr::map(
    cat_vars,
    \(v){
      p_q <- d_orig %>% 
        count(across(all_of(v))) %>% 
        mutate(p_orig = n/sum(n)) %>%
        rename(n_orig = n) %>% 
        full_join(
          d_syn %>% 
            count(across(all_of(v))) %>% 
            mutate(p_syn = n/sum(n)) %>% 
            rename(n_syn = n),
          by = v
        ) %>% 
        mutate(across(c(p_orig, p_syn), ~ifelse(is.na(.), 0, .)))
      
      bhatt = -log(sum(sqrt(p_q$p_orig*p_q$p_syn)))
      hell = philentropy::hellinger(p_q$p_orig, p_q$p_syn, testNA = FALSE)
      
      tibble(modele = name, var = v, dBhatt = bhatt, HD = hell)
    }
  ) %>% 
    purrr::list_rbind()
  
  
  #donne le meme resultat que:
  #philentropy::bhattacharyya(p_q$p_orig, p_q$p_syn, testNA = FALSE, unit = "log", epsilon = 1e-5)
  # return(tibble(name = name, bhatt = bhatt, hell = hell))
}

d_orig = jeudedonnees_SD2011()
cat_vars = d_orig %>% select(where(is.factor)) %>% names()
all_utilite_marges <- imap(all_df1_fcs, dist_marges, d_orig = d_orig, cat_vars = cat_vars) |> list_rbind()


all_utilite_marges %>% 
  filter(modele %in% c("cart","sample","ctree","parametric")) %>% 
  ggplot() +
  geom_point( aes(x = var, y = dBhatt, color = modele, shape = modele), alpha = 0.65, size = 4) +
  scale_color_brewer("Modèle", type = "qual", palette = 2) +
  scale_shape_discrete("Modèle") +
  xlab("") +
  scale_y_continuous("Distance de Bhattacharyya", expand = c(0,0)) +
  coord_flip() +
  theme_minimal(base_size = 20) +
  theme(
    axis.line = element_line(linewidth = 0.45, colour = "grey5"),
    legend.position = c(0.85,0.2), legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5)
  ) 


all_utilite_marges %>% 
  filter(modele %in% c("cart","parametric","sample")) %>% 
  select(modele, var, HD) %>% 
  mutate(var = forcats::fct_reorder(var, HD)) %>% 
  mutate(modele = factor(modele, levels =  c("cart","sample","parametric"), ordered = TRUE)) %>% 
  ggplot() +
  geom_bar( aes(x = var, y = HD, fill = modele), 
            position = "dodge", stat = "identity", alpha = 0.65, width = 0.65) +
  scale_fill_brewer("Modèle", type = "qual", palette = 6) +
  scale_shape_discrete("Modèle") +
  xlab("") +
  scale_y_continuous("Distance de Hellinger", expand = c(0,0)) +
  coord_flip() +
  theme_minimal(base_size = 30) +
  theme(
    axis.line = element_line(linewidth = 0.45, colour = "grey5"),
    legend.position = c(0.85,0.2), legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5)
  ) 


ggsave(filename = "sd2011_hellinger_marges.pdf", device = "pdf", width = 12, height = 10)


all_utilite_marges %>% 
  filter(modele %in% c("cart","ctgan_best","tvae_best")) %>% 
  select(modele, var, HD) %>% 
  mutate(var = forcats::fct_reorder(var, HD)) %>% 
  mutate(modele = factor(modele, levels =  c("cart","ctgan_best","tvae_best"), ordered = TRUE)) %>% 
  ggplot() +
  geom_bar( aes(x = var, y = HD, fill = modele), 
            position = "dodge", stat = "identity", alpha = 0.65, width = 0.65) +
  scale_fill_brewer("Modèle", type = "qual", palette = 6) +
  scale_shape_discrete("Modèle") +
  xlab("") +
  scale_y_continuous("Distance de Hellinger", expand = c(0,0)) +
  coord_flip() +
  theme_minimal(base_size = 30) +
  theme(
    axis.line = element_line(linewidth = 0.45, colour = "grey5"),
    legend.position = c(0.85,0.2), legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5)
  ) 

ggsave(filename = "sd2011_hellinger2_marges.pdf", device = "pdf", width = 12, height = 10)

