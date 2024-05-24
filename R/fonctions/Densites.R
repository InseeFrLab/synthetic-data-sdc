plot_dens_comb <- function(data, variable) {
  p_comb <- ggplot(analyses$data_empile_combined, aes(x = .data[[variable]], color = model)) +
    geom_density() +
    labs(title = paste0("Densité de ", variable,
                        " pour chaque modèle en comparaison à celle pour le jeu de données original"),
         x = variable, y = "Densité") +
    theme_minimal()
  print(p_comb)
}

plot_dens_syn_org <- function(data, variable) {
  liste <- list()
  for (i in 1:length(mes_modeles)) {
    p_all <- ggplot(analyses$data_empile[[i]], aes(x = .data[[variable]], color = "red")) +
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
  
  for (i in 1:length(mes_modeles)) {
    p_bmi <- ggplot(analyses$data_empile[[i]], aes(x = bmi, color = "Synthétisé")) +
      geom_density() +
      geom_density(data = data$original, aes(x = bmi, color = "Original")) +
      geom_density(data = analyses$data_empile[[i]], aes(x = 10000 * weight / height ^ 2, color = "Synthétique")) +
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