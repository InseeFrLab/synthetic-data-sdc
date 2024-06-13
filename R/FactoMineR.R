classement_var <- function(data) {
  data.afdm <- FAMD(data, ncp = 100, graph = FALSE)
  eig.val <- get_eigenvalue(data.afdm)
  
  matrice <- matrix(0, nrow = 1, ncol = length(data))
  for (i in 1:length(data)) {
    var <- 0
    for (j in 1:eig.val[, "variance.percent"]) {
      var <- var + eig.val[, "variance.percent"][j] * data.afdm$var$contrib[names(data)[i], j]
    }
    var <- var / 100
    matrice[1, i] <- var
  }
  colnames(matrice) <- names(data)
  matrice <- matrice[, order(matrice, decreasing = TRUE)]
  return(matrice)
}