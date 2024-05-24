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