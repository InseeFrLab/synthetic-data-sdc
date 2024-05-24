score_propension <- function(data) {
  liste_score <- list()
  for (i in 1:length(mes_modeles)) {
    liste_int <- list()
    for (j in 1:length(data[[i]])) {
      df_org <- data$original
      df_syn <- data[[i]][[j]]
      n1 <- dim(df_org)[1]
      n2 <- dim(df_syn)[1]
      N <- n1 + n2
      cc <- n2 / N
      maxit <- 200

      df.prop <- rbind(df_syn, df_org)
      df.prop <- data.frame(df.prop, t = c(rep(1, n2), rep(0, n1)))

      logit.int <- as.formula(paste("t ~ ."))
      fit <- suppressWarnings(glm(logit.int, data = df.prop, family = "binomial",
                                  control = list(maxit = maxit)))
      score <- predict(fit, type = "response")
      liste_int[[j]] <- score
    }
    liste_score[[i]] <- liste_int
  }
  return(liste_score)
}

#tictoc::tic()
#score_propension(data)
#tictoc::toc()
# 868s

# pMSE = (sum((score - cc)^2, na.rm = T)) / N