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

plot_reg_bmi <- function() {
  var_test_org <- 10000 * (data$original[, "weight"] / (data$original[, "height"])^2)
  reglinorg <- lm(bmi ~ var_test_org, data = data$original)
  
  intercepts <- analyses$reg_coeff["intercept", ]
  slopes <- analyses$reg_coeff["coeff", ]
  
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