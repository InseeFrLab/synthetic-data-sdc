score_propension <- function(data_org, data_syn) {
  n1 <- dim(data_org)[1]
  n2 <- dim(data_syn)[1]
  N <- n1 + n2
  cc <- n2 / N
  maxit <- 200
  
  df.prop <- rbind(data_syn, data_org)
  df.prop <- data.frame(df.prop, t = c(rep(1, n2), rep(0, n1)))
  
  logit.int <- as.formula(paste("t ~ ."))
  fit <- suppressWarnings(glm(logit.int, data = df.prop, family = "binomial",
                              control = list(maxit = maxit)))
  scores <- predict(fit, type = "response")
  pMSE <- (sum((scores - cc)^2, na.rm = T)) / N
  resultats <- list(scores, pMSE)
  names(resultats) <- c("scores", "pMSE")
  return(resultats)
}

#tictoc::tic()
#score_propension(data)
#tictoc::toc()
# 868s

# pMSE = (sum((score - cc)^2, na.rm = T)) / N