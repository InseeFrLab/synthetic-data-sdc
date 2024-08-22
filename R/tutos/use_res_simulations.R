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

analyses <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3_2,
  bucket = BUCKET_SIM_2,
  opts = list("region" = "")
)
names(analyses) <- c("data_empile", "data_original", "data_empile_combined", "table_mean_num",
                     "table_org_mean_num", "table_sd_num", "table_cat", "table_cor",
                     "cor_comp", "somme_cor_mat", "table_mae", "table_mse", "reg_coeff",
                     "utility_measures_all_meth", "nb_repliques_all_meth", "KS_test")

# Calcul mesures d'utilité par méthodes ------------------
tictoc::tic()

utility_measures_all_meth <- imap(
  res_simul[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        synthpop::utility.gen(
          df_synth,
          res_simul$original, 
          method = "cart",
          resamp.method = "none",
          print.flag = FALSE
        )[c("pMSE","SPECKS","PO50","U")] %>% 
          as_tibble() %>% 
          mutate(method = nom_methode)
      }
    )
    plan(sequential)
    return(res %>% list_rbind())
  },
  .progress = TRUE
) %>% list_rbind()

tictoc::toc()
#806.579 sec elapsed

# Quelques graphiques -----------------------------

analyses$utility_measures_all_meth <- analyses$utility_measures_all_meth %>%
  group_by(method) %>% 
  mutate(i = 1:500) %>% 
  mutate(across(pMSE:U, cummean, .names = "{.col}_cummean"))

analyses$utility_measures_all_meth %>% 
  ggplot() +
  geom_line(aes(x = i, y = pMSE_cummean, col = method)) +
  theme_minimal(base_size = 20)

analyses_filtered <- analyses$utility_measures_all_meth %>%
  filter(method != "sample")

analyses_filtered %>%
  ggplot() +
  geom_line(aes(x = i, y = pMSE_cummean, col = method)) +
  theme_minimal(base_size = 20)


utility_graph <- map(
  c("pMSE","SPECKS","PO50","U"),
  \(meth){
    analyses$utility_measures_all_meth %>% 
      ggplot(aes(x=method, y=.data[[meth]], fill=method)) +
      geom_violin(width=1.4) +
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_fill_viridis(discrete = TRUE) +
      coord_flip() +
      theme_ipsum(base_size = 20) +
      theme(
        plot.title = element_text(size=20)
      ) +
      ggtitle(paste0("distribution des ", meth)) +
      ylab(meth) + xlab("méthode")
  }
)
names(utility_graph) <- c("pMSE","SPECKS","PO50","U")

utility_graph$pMSE
utility_graph$SPECKS
utility_graph$PO50
utility_graph$U

utility_measures_summary <- analyses$utility_measures_all_meth %>% 
  group_by(method) %>% 
  summarise(
    across(
      pMSE:U, 
      list(mean = mean, min = min, max = max, 
           pc025 = ~quantile(., probs = 0.025),
           pc975 = ~quantile(., probs = 0.975),
           sd = sd
      ),
      .names = "{.col}_{.fn}"
    )) %>%
  tidyr::pivot_longer(-1, names_to = "indicateur", values_to = "val") %>% 
  tidyr::separate(indicateur, into = c("utility","indicateur"))

utility_measures_summary %>% 
  filter(indicateur == "mean") %>%
  ggplot() +
  geom_bar(aes(x = method, y = val, fill = utility), stat = "identity")+
  coord_flip() +
  facet_wrap(~utility, scales = "free") +
  theme_ipsum(base_size = 20)


# Recherche des répliques -------------------------------------

tictoc::tic()

nb_repliques_all_meth <- imap(
  res_simul[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        tibble(
          n_replicats = inner_join(res_simul$original, df_synth, relationship = "many-to-many") %>% nrow(),
          method = nom_methode
        )
      }
    )
    plan(sequential)
    return(res %>% list_rbind())
  },
  .progress = TRUE
) %>% list_rbind()

tictoc::toc()
#806.579 sec elapsed


nb_repliques_all_meth %>% 
  group_by(method) %>% 
  summarise(across(n_replicats, list(mean=mean, min=min, max=max, sd=sd)))




