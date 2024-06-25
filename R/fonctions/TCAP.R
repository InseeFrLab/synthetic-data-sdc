calc_WEAP <- function(data_syn, K_s, T_s) {
  n <- nrow(data_syn)
  WEAP <- rep(0, n)
  
  for (i in 1:n) {
    K_i <- data_syn[i, K_s]
    T_i <- data_syn[i, T_s]
    
    num <- sum(T_i == data_syn[, T_s] & apply(data_syn[, K_s], 1, function(row) all(row == K_i)))
    denom <- sum(apply(data_syn[, K_s], 1, function(row) all(row == K_i)))
    
    if (denom == 0) {
      WEAP[i] <- 0
    } else {
      WEAP[i] <- num / denom
    }
  }
  
  return(WEAP)
}

filter_WEAP_1 <- function(data_syn, WEAP) {
  data_syn_filtered <- data_syn[WEAP == 1, ]
  return(data_syn_filtered)
}

calc_TCAP <- function(data_org, data_syn_filtered, K_s, T_s) {
  n <- nrow(data_syn_filtered)
  TCAP <- rep(0, n)
  
  for (i in 1:n) {
    K_i <- data_syn_filtered[i, K_s]
    T_i <- data_syn_filtered[i, T_s]
    
    num <- sum(T_i == data_org[, T_s] & apply(data_org[, K_s], 1, function(row) all(row == K_i)))
    denom <- sum(apply(data_org[, K_s], 1, function(row) all(row == K_i)))
    
    if (denom == 0) {
      TCAP[i] <- NA
    } else {
      TCAP[i] <- num / denom
    }
  }
  
  return(TCAP)
}

data_org <- data$original
data_syn <- data$cart[[1]]
K_s <- c("sex", "age")
T_s <- "socprof"

tictoc::tic()
WEAP <- calc_WEAP(data_syn, K_s, T_s)
tictoc::toc()
# 3390.303 sec elapsed

data_syn_filtered <- filter_WEAP_1(data_syn, WEAP)

tictoc::tic()
TCAP <- calc_TCAP(data_org, data_syn_filtered, K_s, T_s) #  sec elapsed
tictoc::toc()
# 128.286 sec elapsed

print(TCAP)
print(mean(TCAP, na.rm = TRUE))

# Tests ------------------------------------------------------------------------

r <- sum(apply(data_syn[, K_s], 1, function(row) all(row == K_i)))


