KS_test <- function(data) {
  liste <- list()
  mat_recap <- matrix(0, nrow = length(mes_modeles), ncol = length(num))
  for (i in 1:length(mes_modeles)) {
    ks_mat <- matrix(0, nrow = 500, ncol = length(num))
    ks_stats <- matrix(0, nrow = length(num), ncol = 10)
    for (j in 1:length(data[[i]])) {
      for (k in 1:length(num)) {
        ks_mat[j, k] <- suppressWarnings(ks.test(data[[i]][[j]][, num[k]], data$original[, num[k]])$p.value)
      }
    }
    mat_recap[i, ] <- colMeans(ks_mat)
    
    for (l in 1:length(num)) {
      ks_stats[l, 1] <- min(ks_mat[, l])
      ks_stats[l, 2] <- max(ks_mat[, l])
      ks_stats[l, 3] <- median(ks_mat[, l])
      ks_stats[l, 4] <- mean(ks_mat[, l])
      ks_stats[l, 5] <- sd(ks_mat[, l])
      ks_stats[l, 6] <- mean(ks_mat[, l]) / sd(ks_mat[, l])
      ks_stats[l, 7] <- quantile(ks_mat[, l], 0.025)
      ks_stats[l, 8] <- quantile(ks_mat[, l], 0.25)
      ks_stats[l, 9] <- quantile(ks_mat[, l], 0.75)
      ks_stats[l, 10] <- quantile(ks_mat[, l], 0.975)
    }
    row.names(ks_stats) <- num
    colnames(ks_stats) <- c("min", "max", "mediane", "moyenne", "sd", "cv", "q025", "q1", "q3", "q975")
    liste[[i]] <- ks_stats
  }
  row.names(mat_recap) <- mes_modeles
  colnames(mat_recap) <- num
  names(liste) <- mes_modeles
  return(list(mat_recap, liste))
}
