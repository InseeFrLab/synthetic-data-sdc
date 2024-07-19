if (!requireNamespace("vcd", quietly = TRUE)) install.packages("vcd"); library(vcd)
if (!requireNamespace("polycor", quietly = TRUE)) install.packages("polycor"); library(polycor)

mixed_correlations <- function(data) {
  n <- ncol(data)
  cor_matrix <- matrix(NA, n, n)
  colnames(cor_matrix) <- colnames(data)
  rownames(cor_matrix) <- colnames(data)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        cor_matrix[i, j] <- 1
      } else {
        x <- data[[i]]
        y <- data[[j]]
        
        if (is.numeric(x) && is.numeric(y)) {
          cor_matrix[i, j] <- cor(x, y)
        } else if (is.factor(x) && is.factor(y)) {
          cor_matrix[i, j] <- assocstats(table(x, y))$cramer
        } else if ((is.numeric(x) && is.factor(y)) || (is.factor(x) && is.numeric(y))) {
          cor_matrix[i, j] <- hetcor(data.frame(x, y))$correlations[1, 2]
        }
      }
    }
  }
  return(cor_matrix)
}