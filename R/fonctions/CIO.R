CIO = function(df_org, df_syn, variable) {
  moy_org = mean(df_org[, variable])
  moy_syn = mean(df_syn[, variable])
  
  sd_org = sd(df_org[, variable])
  sd_syn = sd(df_syn[, variable])
  
  sqrt_org = sqrt(length(df_org[, variable]))
  sqrt_syn = sqrt(length(df_syn[, variable]))
  
  t = qt(0.975, df = length(df_org[, variable]) - 1)
  
  lo = moy_org - t * sd_org / sqrt_org
  uo = moy_org + t * sd_org / sqrt_org
  ls = moy_syn - t * sd_syn / sqrt_syn
  us = moy_syn + t * sd_syn / sqrt_syn
  
  cio = 0.5 * (((min(uo, us) - max(lo, ls))/(uo - lo))+((min(uo, us) - max(lo, ls))/(us - ls)))
  return(cio)
}

stat_CIO <- function(df_org, data_syn, variable) {
  mat_CIO <- matrix(0, nrow = length(mes_modeles), ncol = 10)
  row.names(mat_CIO) <- mes_modeles
  colnames(mat_CIO) <- c("min", "max", "median", "mean", "sd", "cv", "q025", "q1", "q3", "q975")
  for (i in 1:length(mes_modeles)) {
    vec <- rep(0, length(data_syn[[i]]))
    for (j in 1:length(data_syn[[i]])) {
      vec[j] <- CIO(df_org, data_syn[[i]][[j]], variable)
    }
    mat_CIO[i, 1] <- round(min(vec), digits = 3)
    mat_CIO[i, 2] <- round(max(vec), digits = 3)
    mat_CIO[i, 3] <- round(median(vec), digits = 3)
    mat_CIO[i, 4] <- round(mean(vec), digits = 3)
    mat_CIO[i, 5] <- round(sd(vec)/mean(vec), digits = 3)
    mat_CIO[i, 6] <- round(median(vec), digits = 3)
    mat_CIO[i, 7] <- round(quantile(vec, 0.025), digits = 3)
    mat_CIO[i, 8] <- round(quantile(vec, 0.25), digits = 3)
    mat_CIO[i, 9] <- round(quantile(vec, 0.75), digits = 3)
    mat_CIO[i, 10] <- round(quantile(vec, 0.975), digits = 3)
  }
  return(mat_CIO)
}

# Tests ------------------------------------------------------------------------






