KS_test <- function(data) {
  mat <- matrix(0, nrow = length(mes_modeles), ncol = length(num))
  for (i in 1:length(mes_modeles)) {
    ks_mat <- matrix(0, nrow = 500, ncol = length(num))
    for (j in 1:length(data[[i]])) {
      for (k in 1:length(num)) {
        ks_mat[j, k] <- suppressWarnings(ks.test(data[[i]][[j]][, num[k]], data$original[, num[k]])$p.value)
      }
    }
    mat[i, ] <- colMeans(ks_mat)
  }
  row.names(mat) <- mes_modeles
  colnames(mat) <- num
  return(mat)
}
