library(aws.s3)
library(synthpop)
library(purrr)
library(furrr)
library(dplyr)
library(tidyverse)
library(tictoc)
library(ggplot2)
library(viridis)
library(abind)
library(plotly)
library(wesanderson)
library(pracma)
library(hrbrthemes)

# Import des données -----------------------------------------------------------
FILE_KEY_IN_S3 <- "20240512_sim_synthpop_sample_cart_ctree_parametric_bag_rf_500_sims.RDS"
BUCKET = "projet-donnees-synthetiques"
BUCKET_SIM = file.path(BUCKET, "simulations")

data <- aws.s3::s3read_using(
  FUN = readRDS,
  object = FILE_KEY_IN_S3,
  bucket = BUCKET_SIM,
  opts = list("region" = "")
)
str(data, max.level=1)
methodes <- which(names(data) != "original")

mes_modeles <- c("sample", "cart", "ctree", "parametric", "bag", "rf")

n_sim <- 500
num_seed <- 40889
options(max.print = 10000)

varsnum = c("age", "depress", "nofriend", "height", "weight", "bmi")
num = c("age", "nofriend", "height", "weight", "bmi")
fac = c("sex", "agegr", "placesize", "edu", "socprof", "marital", "ls", "depress",
        "trust", "trustfam", "trustneigh", "sport", "smoke", "alcabuse", "alcsol", "wkabint", "englang")

# Modifier le type de depress en fonction du besoin
#data$sample[[1]][, "depress"] <- factor(data$sample[[1]][, "depress"], levels = 0:21)
#data$sample[[1]][, "depress"] <- as.numeric(data$sample[[1]][, "depress"])

# Stats variable numériques ----------------------------------------------------
data_empile <- map(data[methodes], \(df_list) df_list %>%
                     imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())

calc_stats_mean_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ mean(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
} 

table_mean_num <- imap(data_empile, \(df, methode) calc_stats_mean_num(df) %>%
                         mutate(methode = methode)) %>% list_rbind() 


calc_org_mean_num <- function(df) {
  df %>% summarise(
    across(where(is.numeric), list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975)
    )))
}

table_org_mean_num <- calc_stats_mean_num(data$original)

calc_stats_sd_num <- function(df) {
  df %>% group_by(index_sim) %>%
    summarise(
      across(where(is.numeric), ~ sd(.)
             , .names = "{.col}"), .groups = "drop"
    ) %>% summarise(across(-index_sim, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}

table_sd_num <- imap(data_empile, \(df, methode) calc_stats_sd_num(df) %>%
                       mutate(methode = methode)) %>% list_rbind() 


# Stats variable catégorielles -------------------------------------------------

calc_stats_cat <- function(df) {
  cat_stats <- df %>%
    select(index_sim, where(is.factor)) %>%
    pivot_longer(cols = -index_sim, names_to = "variable", values_to = "modalites") %>%
    group_by(index_sim, variable, modalites) %>%
    summarise(freq = n(), .groups = "drop") %>%
    group_by(variable, modalites) %>%
    summarise(across(freq, list(
      min = ~ min(.),
      max = ~ max(.),
      mean = ~ mean(.),
      sd = ~ sd(.),
      cv = ~ sd(.) / mean(.),
      median = ~ median(.),
      q1 = ~ quantile(., 0.25),
      q3 = ~ quantile(., 0.75),
      q025 = ~ quantile(., 0.025),
      q975 = ~ quantile(., 0.975))))
}

table_cat <- imap(data_empile, \(df, methode) calc_stats_cat(df) %>%
                    mutate(methode = methode)) %>% list_rbind() 


# Correlations -----------------------------------------------------------------

calc_correlation <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    cor(use = "complete.obs")
}

table_cor <- map(
  data[methodes],
  \(list_df){
    array_cor <- map(
      list_df,
      \(df){
        calc_correlation(df)
      }
    ) %>% abind::abind(along = 3) 
    list(mean = array_cor %>% apply(MARGIN = 1:2, FUN = mean),
         sd = array_cor %>% apply(MARGIN = 1:2, FUN = sd))
  }
)

cor_comp = list()
for (i in 1:length(mes_modeles)) {
  cor_comp[[i]] = abs(table_cor[[i]][[1]] - cor(data$original[, varsnum]))
}
names(cor_comp) <- mes_modeles

somme_cor_mat = matrix(0, nrow = 1, ncol = 6)
for (i in 1:length(mes_modeles)) {
  somme_cor_mat[i] = sum(cor_comp[[i]])
}
colnames(somme_cor_mat) = mes_modeles

# MAE --------------------------------------------------------------------------

calc_mae <- function(res) {
  mae_table = matrix(0, nrow = 6, ncol = 17)
  
  for (i in 1:length(res[methodes])) {
    mae_table_meth = matrix(0, nrow = 500, ncol = 17)
    for (j in 1:length(res[[i]])) {
      res[[i]][[j]][, "depress"] <- factor(res[[i]][[j]][, "depress"], levels = 0:21)
      for (k in 1:length(fac)) {
        MAE <- mean(abs(table(res$original[[fac[k]]]) - table(res[[i]][[j]][[fac[k]]])))
        mae_table_meth[j, k] <- MAE
      }
    }
    mae_table[i, ] <- colMeans(mae_table_meth)
  }
  
  row.names(mae_table) = mes_modeles
  colnames(mae_table) = fac
  return(mae_table)
}

table_mae <- calc_mae(data)

# MSE --------------------------------------------------------------------------

calc_mse <- function(res) {
  mse_table = matrix(0, nrow = 6, ncol = 5)
  
  for (i in 1:length(res[methodes])) {
    mse_table_meth = matrix(0, nrow = 500, ncol = 5)
    for (j in 1:length(res[[i]])) {
      for (k in 1:length(num)) {
        MSE <- mean((res$original[[num[k]]] - res[[i]][[j]][[num[k]]])^2)
        mse_table_meth[j, k] <- MSE
      }
    }
    mse_table[i, ] <- colMeans(mse_table_meth)
  }
  
  row.names(mse_table) = mes_modeles
  colnames(mse_table) = num
  return(mse_table)
}

table_mse <- calc_mse(data)

# Analyse bmi ------------------------------------------------------------------
bmi_comp <- function(res) {
  matrice = matrix(0, nrow = 2, ncol = 6)
  for (i in 1:length(res[methodes])) {
    mat_cal = matrix(0, nrow = 500, ncol = 2)
    for (j in 1:length(res[[i]])) {
      mat_cal[j, 1] = mean(10000 * (res[[i]][[j]][, "weight"] / (res[[i]][[j]][, "height"])^2) - res$original[, "bmi"])
      mat_cal[j, 2] = mean(res[[i]][[j]][, "bmi"] - res$original[, "bmi"])
    }
    matrice[1, i] = sum(mat_cal[, 1])
    matrice[2, i] = sum(mat_cal[, 2])
  }
  
  colnames(matrice) = mes_modeles
  row.names(matrice) = c("Comparaison synth/synth", "Comparaison synth/org")
  return(matrice)
}

reg_coeff <- matrix(0, nrow = 2, ncol = length(mes_modeles))
for (i in 1:length(mes_modeles)) {
  coeff = matrix(0, nrow = length(data[[i]]), ncol = 2)
  for (j in 1:length(data[[i]])) {
    var_test = 10000 * (data[[i]][[j]][, "weight"] / (data[[i]][[j]][, "height"])^2)
    reglin = lm(bmi ~ var_test, data = data[[i]][[j]])
    coeff[j, 1] = reglin$coefficients[1]
    coeff[j, 2] = reglin$coefficients[2]
  }
  reg_coeff[1, i] = mean(coeff[, 1])
  reg_coeff[2, i] = mean(coeff[, 2])
}
row.names(reg_coeff) <- c("intercept", "coeff")
colnames(reg_coeff) <- mes_modeles


plot_reg_bmi <- function() {
  var_test_org <- 10000 * (data$original[, "weight"] / (data$original[, "height"])^2)
  reglinorg <- lm(bmi ~ var_test_org, data = data$original)
  
  intercepts <- reg_coeff["intercept", ]
  slopes <- reg_coeff["coeff", ]
  
  x_vals <- seq(0, 100, length.out = 100)
  
  pltreg <- plot(x_vals, x_vals, type = "n", xlab = "x", ylab = "y", xlim = c(0, 100), ylim = c(-5, 100))
  colors <- rainbow(length(mes_modeles))
  for (i in 1:length(mes_modeles)) {
    y_vals <- intercepts[i] + slopes[i] * x_vals
    lines(x_vals, y_vals, col = colors[i], lwd = 1)
  }
  y_org <- reglinorg$coefficients[1] + reglinorg$coefficients[2] * x_vals
  lines(x_vals, y_org, col = "black", lwd = 0.5)
  legend("bottomright", legend = mes_modeles, col = colors, lwd = 2, title = "Modèles")
  title("Droites de Régression")
  print(pltreg)
}

plot_nuage_bmi <- function(modele, i) {
  var_height_mod <- data[[modele]][[i]]$weight ^ 2 / 10000
  nuage_bmi <- plot_ly(x = data[[modele]][[i]]$weight,
                       y = var_height_mod,
                       z = data[[modele]][[i]]$bmi,
                       type = "scatter3d",
                       mode = "markers",
                       color = data[[modele]][[i]]$bmi)
}

# Densités ---------------------------------------------------------------------
plot_dens_comb <- function(data, variable) {
  data_empile <- map(data[methodes], \(df_list) df_list %>%
                       imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())
  
  data_empile_combined <- map(mes_modeles, \(model) {
    data_empile[[model]] %>% mutate(model = model)
  })
  data_original <- data$original %>% mutate(model = "original")
  data_empile_combined <- bind_rows(data_empile_combined, data_original)
  
  ggplot(data_empile_combined, aes(x = .data[[variable]], color = model)) +
    geom_density() +
    labs(title = paste0("Densité de ", variable,
                        " pour chaque modèle en comparaison à celle pour le jeu de données original"),
                        x = variable, y = "Densité") +
    theme_minimal()
}

plot_dens_syn_org <- function(data, variable) {
  liste <- list()
  
  data_empile <- map(data[methodes], \(df_list) df_list %>%
                       imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())
  
  data_empile_combined <- map(mes_modeles, \(model) {
    data_empile[[model]] %>% mutate(model = model)
  })
  
  for (i in 1:length(mes_modeles)) {
    p_all <- ggplot(data_empile[[i]], aes(x = .data[[variable]], color = "red")) +
      geom_density() +
      geom_density(data = data$original, aes(x = .data[[variable]]), color = "black") +
      labs(title = paste0("Densité de ", variable, " pour le modèle ", mes_modeles[i]), x = variable, y = "Densité") +
      theme_minimal()
    liste[[i]] <- p_all
  }
  
  return(liste)
}

dens_bmi <- function() {
  liste_bmi <- list()
  data_empile <- map(data[methodes], \(df_list) df_list %>%
                       imap(\(df, i) df %>% mutate(index_sim = i)) %>% bind_rows())
  
  data_empile_combined <- map(mes_modeles, \(model) {
    data_empile[[model]] %>% mutate(model = model)
  })
  
  for (i in 1:length(mes_modeles)) {
    p_bmi <- ggplot(data_empile[[i]], aes(x = bmi, color = "Synthétisé")) +
      geom_density() +
      geom_density(data = data$original, aes(x = bmi, color = "Original")) +
      geom_density(data = data_empile[[i]], aes(x = 10000 * weight / height ^ 2, color = "Synthétique")) +
      labs(title = paste0("Densité de bmi originale vs synthétique vs synthétisée pour le modèle ",
                          mes_modeles[i]), x = "bmi", y = "Densité") +
      theme_minimal() +
      scale_color_manual(name = "Variantes",
                         breaks = c("Synthétisé", "Original", "Synthétique"),
                         values = c("Synthétisé" = "red", "Original" = "black", "Synthétique" = "cyan"))
    
    
    liste_bmi[[i]] <- p_bmi
  }
  
  return(liste_bmi)
}

densite_diff <- function(data, variable) {
  aires <- matrix(0, nrow = 3, ncol = length(mes_modeles))
  aires_diff <- matrix(0, nrow = 500, ncol = 6)
  for (i in 1:length(mes_modeles)) {
    for (j in 1:length(data[[i]])) {
      densite_syn <- density(data[[i]][[j]][, variable])
      densite_original <- density(data$original[, variable])
      aire_syn <- trapz(densite_syn$x, densite_syn$y)
      aire_original <- trapz(densite_original$x, densite_original$y)
      aires_diff[j, i] <- aire_original - aire_syn
    }
    aires[1, i] <- mean(aires_diff[, i])
    aires[2, i] <- sd(aires_diff[, i])
    aires[3, i] <- sum(abs(aires_diff[, i]))
  }
  colnames(aires) <- mes_modeles
  colnames(aires_diff) <- mes_modeles
  row.names(aires) <- c("Moyenne différence d'aires", "Ecart-type différence d'aires",
                        "Somme différence d'aires")
  return(list(aires, aires_diff))
}


bp_densite <- function(data, variable) {
  result <- densite_diff(data, variable)
  
  aires_diff_long <- as.data.frame(result[[2]])
  aires_diff_long <- aires_diff_long %>%
    mutate(Observation = 1:nrow(aires_diff_long)) %>%
    pivot_longer(cols = -Observation, names_to = "modele", values_to = "valeur")
  
  ggplot(aires_diff_long, aes(x = modele, y = valeur)) +
    geom_boxplot() +
    labs(title = paste0("Boxplots des différences d'aires pour la variable ", variable),
         x = "Modèles",
         y = "Différence d'aires") +
    theme_minimal()
}

recap_diff_dens <- function(data) {
  recap <- matrix(0, nrow = 6, ncol = length(varsnum))
  for (i in 1:length(varsnum)) {
    dens <- densite_diff(data, varsnum[i])[[1]]
    for (j in 1:length(mes_modeles)) {
      recap[j, i] <- dens[1, j]
    }
  }
  colnames(recap) <- varsnum
  row.names(recap) <- mes_modeles
  return(recap)
}

# Utilite ----------------------------------------------------------------------

utility_measures_all_meth <- imap(
  data[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        synthpop::utility.gen(
          df_synth,
          data$original, 
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

moycum_mesures <- function() {
  utility_measures_all_meth <- utility_measures_all_meth %>%
    group_by(method) %>% 
    mutate(i = 1:500) %>% 
    mutate(across(pMSE:U, cummean, .names = "{.col}_cummean"))
  
  utility_measures_all_meth %>% 
    ggplot() +
    geom_line(aes(x = i, y = pMSE_cummean, col = method))
}

distribution_mesures <- function() {
  utility_graph <- map(
    c("pMSE","SPECKS","PO50","U"),
    \(meth){
      utility_measures_all_meth %>% 
        ggplot( aes(x=method, y=.data[[meth]], fill=method)) +
        geom_violin(width=1.4) +
        geom_boxplot(width=0.1, color="grey", alpha=0.2) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        theme_ipsum() +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle(paste0("distribution des ", meth)) +
        ylab(meth) + xlab("méthode")
    }
  )
  names(utility_graph) <- c("pMSE","SPECKS","PO50","U")
  
  return(list(utility_graph$pMSE, utility_graph$SPECKS, utility_graph$PO50, utility_graph$U))
}

resume_mesures <- function() {
  utility_measures_summary <- utility_measures_all_meth %>% 
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
    theme_ipsum()
}

# Recherche des répliques ------------------------------------------------------

nb_repliques_all_meth <- imap(
  data[methodes],
  \(res_one_methode, nom_methode){
    plan(multisession, workers = 10)
    res <- future_map(
      res_one_methode,
      \(df_synth){
        tibble(
          n_replicats = inner_join(data$original, df_synth, relationship = "many-to-many") %>% nrow(),
          method = nom_methode
        )
      }
    )
    plan(sequential)
    return(res %>% list_rbind())
  },
  .progress = TRUE
) %>% list_rbind()


# Scores de propensions --------------------------------------------------------
score_propension <- function(data) {
  liste_score <- list()
  for (i in 1:length(mes_modeles)) {
    liste_int <- list()
    for (j in 1:length(data[[i]])) {
      df_org <- data$original
      df_syn <- data[[i]][[j]]
      n1 <- dim(df_org)[1]
      n2 <- dim(df_syn)[1]
      N <- n1 + n2
      cc <- n2 / N
      maxit <- 200
      
      df.prop <- rbind(df_syn, df_org)
      df.prop <- data.frame(df.prop, t = c(rep(1, n2), rep(0, n1)))
      
      logit.int <- as.formula(paste("t ~ ."))
      fit <- suppressWarnings(glm(logit.int, data = df.prop, family = "binomial",
                                  control = list(maxit = maxit)))
      score <- predict(fit, type = "response")
      liste_int[[j]] <- score
    }
    liste_score[[i]] <- liste_int
  }
  return(liste_score)
}

tictoc::tic()
score_propension(data)
tictoc::toc()

pMSE = (sum((score - cc)^2, na.rm = T)) / N


# Tests ------------------------------------------------------------------------









