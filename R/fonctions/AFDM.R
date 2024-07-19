classement_var <- function(data, data.afdm) {
  eig.val <- data.afdm$eig
  matrice <- matrix(0, nrow = 1, ncol = length(data))
  for (i in 1:length(data)) {
    var <- 0
    for (j in 1:eig.val[, 2]) {
      var <- var + eig.val[, 2][j] * data.afdm$var$contrib[names(data)[i], j]
    }
    matrice[1, i] <- var
  }
  colnames(matrice) <- names(data)
  matrice <- matrice[, order(matrice, decreasing = TRUE)]
  ordre <- names(matrice)
  return(list(matrice, ordre))
}

varexp <- function(data.afdm) {
  barplot(data.afdm$eig[, 2], 
          names.arg = 1:nrow(data.afdm$eig), 
          main = "Variance expliquée par dimensions (%)",
          xlab = "Principales dimensions",
          ylab = "Pourcentage de variance",
          col ="steelblue")
  lines(x = 1:nrow(data.afdm$eig), data.afdm$eig[, 2], 
        type = "b", pch = 19, col = "red")
  return(barplot)
}