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