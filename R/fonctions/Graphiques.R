bmi_comp <- function(res) {
  matrice = matrix(0, nrow = 2, ncol = 6)
  for (i in 1:length(res[methodes])) {
    mat_cal = matrix(0, nrow = 500, ncol = 2)
    for (j in 1:length(res[[i]])) {
      mat_cal[j, 1] = mean(abs(10000 * (res[[i]][[j]][, "weight"] / (res[[i]][[j]][, "height"])^2) - res$original[, "bmi"]))
      mat_cal[j, 2] = mean(abs(res[[i]][[j]][, "bmi"] - res$original[, "bmi"]))
    }
    matrice[1, i] = sum(mat_cal[, 1])
    matrice[2, i] = sum(mat_cal[, 2])
  }
  
  colnames(matrice) = mes_modeles
  row.names(matrice) = c("bmi synthétique vs bmi original", "bmi synthétisé vs bmi original")
  return(matrice)
}

plot_reg_bmi <- function() {
  var_test_org <- 10000 * (data$original[, "weight"] / (data$original[, "height"])^2)
  reglinorg <- lm(bmi ~ var_test_org, data = data$original)
  
  intercepts <- analyses$reg_coeff["intercept", ]
  slopes <- analyses$reg_coeff["coeff", ]
  
  x_vals <- seq(0, 100, length.out = 100)
  
  pltreg <- plot(x_vals, x_vals, type = "n", xlab = "bmi original", ylab = "bmi synthétique",
                 xlim = c(min(data$original[, "bmi"]), max(data$original[, "bmi"])),
                 ylim = c(min(data$original[, "bmi"]), max(data$original[, "bmi"])))
  colors <- rainbow(length(mes_modeles))
  for (i in 1:length(mes_modeles)) {
    y_vals <- intercepts[i] + slopes[i] * x_vals
    lines(x_vals, y_vals, col = colors[i], lwd = 1)
  }
  y_org <- reglinorg$coefficients[1] + reglinorg$coefficients[2] * x_vals
  lines(x_vals, y_org, col = "black", lwd = 1, lty = "dashed")
  legend("topleft", legend = mes_modeles, col = colors, lwd = 2, title = "Modèles")
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

CDF <- function(data, modele, variable, datasetnum) {
  p_cdf <- ggplot(data[[modele]][[datasetnum]], aes(x = .data[[variable]])) +
    stat_ecdf(geom = "step") +
    labs(title = paste0("Fonction de répartition empirique de la variable ", variable,
                        " pour le modele ", modele, "\n pour le jeu de données ", datasetnum),
                                  y = paste0("F(", variable, ")"), x = variable) +
    theme_classic()
    
  print(p_cdf)
}










