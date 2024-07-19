VCramer <- function(data, variable) {
  liste_vcramer <- list()
  for (i in 1:length(mes_modeles)) {
    for (j in 1:length(data[[i]])) {
      data[[i]][[j]][, variable] <- factor(data[[i]][[j]][, variable], levels = 0:21)
      mat_vcramer <- matrix(0, nrow = length(fac), ncol = length(fac))
      for (k in 1:length(fac)) {
        for (l in 1:length(fac)) {
          mat_vcramer[k, l] <- cramerV(data[[i]][[j]][, k], data[[i]][[j]][, l])
        }
      }
      row.names(mat_vcramer) <- fac
      colnames(mat_vcramer) <- fac
      liste_vcramer[[i]][[j]] <- mat_vcramer
    }
    
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

VCramer1 <- function(data, variable) {
  liste_vcramer <- list()
  for (i in 1:length(mes_modeles)) {
    for (j in 1:length(data[[i]])) {
      data[[i]][[j]][, variable] <- factor(data[[i]][[j]][, variable], levels = 0:21)
      mat_vcramer <- matrix(0, nrow = length(fac), ncol = length(fac))
      for (k in 1:length(fac)) {
        for (l in 1:length(fac)) {
          mat_vcramer[k, l] <- cramerV(data[[i]][[j]][, k], data[[i]][[j]][, l])
        }
      }
      row.names(mat_vcramer) <- fac
      colnames(mat_vcramer) <- fac
      liste_vcramer[[i]][[j]] <- mat_vcramer
    }
  }
  return(liste_vcramer)
}
