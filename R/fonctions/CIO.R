CIO = function(df_org, df_syn) {
  liste = rep(0, length(df_org))
  for (i in 1:length(df_org)) {
    moy_org = mean(df_org[, names(df_org)[i]])
    moy_syn = mean(df_syn[, names(df_syn)[i]])
    
    sd_org = sd(df_org[, names(df_org)[i]])
    sd_syn = sd(df_syn[, names(df_syn)[i]])
    
    sqrt_org = sqrt(length(df_org[, names(df_org)[i]]))
    sqrt_syn = sqrt(length(df_syn[, names(df_syn)[i]]))
    
    t = qt(0.975, df = length(df_org[, names(df_org)[i]]) - 1)
    
    lo = moy_org - t * sd_org / sqrt_org
    uo = moy_org + t * sd_org / sqrt_org
    ls = moy_syn - t * sd_syn / sqrt_syn
    us = moy_syn + t * sd_syn / sqrt_syn
    
    liste[i] = 0.5 * (((min(uo, us) - max(lo, ls))/(uo - lo))+((min(uo, us) - max(lo, ls))/(us - ls)))
  }
  return(mean(liste))
}

stat_CIO <- function() {
  mat_CIO <- matrix(0, nrow = length(mes_modeles), ncol = 10)
  row.names(mat_CIO) <- mes_modeles
  colnames(mat_CIO) <- c("min", "max", "median", "mean", "sd", "cv", "q025", "q1", "q3", "q975")
  for (i in 1:length(mes_modeles)) {
    vec <- rep(0, length(data[[i]]))
    for (j in 1:length(data[[i]])) {
      vec[j] <- CIO(data$original[, num], data[[i]][[j]][, num])
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
