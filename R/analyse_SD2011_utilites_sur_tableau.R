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

utility_global <- synthpop::utility.gen(all_df1_fcs, df, print.stats = "all")

tableau1 <- c("sex","agegr","edu","placesize","smoke")
utility_tableau1 <- synthpop::utility.tab(all_df1_fcs, df, vars = tableau1, print.stats = "all")
synthpop::utility.tab(all_df1_fcs$cart, df, vars = tableau1, print.stats = "all")

dist_bhatt <- function(dat, name = ""){
  p_q <- df %>% 
    count(across(all_of(tableau1))) %>% 
    mutate(p_orig = n/sum(n)) %>%
    rename(n_orig = n) %>% 
    full_join(
      dat %>% 
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

all_bhatt <- imap(all_df1_fcs, dist_bhatt) |> list_rbind()







