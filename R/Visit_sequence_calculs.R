library(synthpop)
library(timeR)
source("~/work/synthetic-data-sdc/R/fonctions/creation_jeu.R")

# Données
data <- jeudedonnees_SD2011()
data[, "depress"] <- factor(data[, "depress"], levels = 0:21)
num <- c("age", "nofriend", "height", "weight", "bmi")
fac <- c("sex", "agegr", "placesize", "edu", "socprof", "marital", "ls", "depress",
         "trust", "trustfam", "trustneigh", "sport", "smoke", "alcabuse", "alcsol",
         "wkabint", "englang")
data_num <- data[, num]
data_fac <- data[, fac]

mat_resu <- matrix(0, nrow = 2, ncol = 10)
row.names(mat_resu) <- c("utilite", "temps")
colnames(mat_resu) <- c("classique", "numfac1", "numfac2", "afdm", "aleatoire",
                        "noncorrele", "num", "nummoda", "fac", "facmoda")

timer = createTimer(precision = "ms")

# Classique
mat_classique <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_classique) <- c("utilite", "temps")
for (i in 1:50) {
  timer$start("classique")
  syn_classique <- syn(data, seed = i)
  timer$stop("classique")
  
  pMSE_classique <- utility.gen(syn_classique, data)$pMSE
  
  mat_classique[1, i] <- pMSE_classique
  mat_classique[2, i] <- as.integer(timer$getTimeElapsed("classique"))
}
mat_resu[1, 1] <- mean(mat_classique[1, ])
mat_resu[2, 1] <- mean(mat_classique[2, ])

# Num avant fac
mat_numfac1 <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_numfac1) <- c("utilite", "temps")
for (i in 1:50) {
  vs_numfac1 <- c(2,14,20,21,22,1,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19)
  timer$start("numfac1")
  syn_numfac1 <- syn(data, visit.sequence = vs_numfac1, seed = i)
  timer$stop("numfac1")
  
  pMSE_numfac1 <- utility.gen(syn_numfac1, data)$pMSE
  
  mat_numfac1[1, i] <- pMSE_numfac1
  mat_numfac1[2, i] <- as.integer(timer$getTimeElapsed("numfac1"))
}
mat_resu[1, 2] <- mean(mat_numfac1[1, ])
mat_resu[2, 2] <- mean(mat_numfac1[2, ])

# Num
mat_num <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_num) <- c("utilite", "temps")
for (i in 1:50) {
  timer$start("num")
  syn_num <- syn(data[, num], seed = i)
  timer$stop("num")
  
  pMSE_num <- utility.gen(syn_num, data[, num])$pMSE
  
  mat_num[1, i] <- pMSE_num
  mat_num[2, i] <- as.integer(timer$getTimeElapsed("num"))
}
mat_resu[1, 7] <- mean(mat_num[1, ])
mat_resu[2, 7] <- mean(mat_num[2, ])

# Fac
mat_fac <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_fac) <- c("utilite", "temps")
for (i in 1:50) {
  timer$start("fac")
  syn_fac <- syn(data[, fac], seed = i)
  timer$stop("fac")
  
  pMSE_fac <- utility.gen(syn_fac, data[, fac])$pMSE
  
  mat_fac[1, i] <- pMSE_fac
  mat_fac[2, i] <- as.integer(timer$getTimeElapsed("fac"))
}
mat_resu[1, 9] <- mean(mat_fac[1, ])
mat_resu[2, 9] <- mean(mat_fac[2, ])

# Num classé par nombre de modalités
mat_nummoda <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_nummoda) <- c("utilite", "temps")
for (i in 1:50) {
  vs_nummoda <- c(2,3,1,4,5)
  timer$start("nummoda")
  syn_nummoda <- syn(data[, num], visit.sequence = vs_nummoda, seed = i)
  timer$stop("nummoda")
  
  pMSE_nummoda <- utility.gen(syn_nummoda, data[, num])$pMSE
  
  mat_nummoda[1, i] <- pMSE_nummoda
  mat_nummoda[2, i] <- as.integer(timer$getTimeElapsed("nummoda"))
}
mat_resu[1, 8] <- mean(mat_nummoda[1, ])
mat_resu[2, 8] <- mean(mat_nummoda[2, ])

# Fac classé par nombre de modalités
mat_facmoda <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_facmoda) <- c("utilite", "temps")
for (i in 1:50) {
  vs_facmoda <- c(14,15,1,13,12,17,9,10,11,16,4,2,6,3,7,5,8)
  timer$start("facmoda")
  syn_facmoda <- syn(data[, fac], visit.sequence = vs_facmoda, seed = i)
  timer$stop("facmoda")
  
  pMSE_facmoda <- utility.gen(syn_facmoda, data[, fac])$pMSE
  
  mat_facmoda[1, i] <- pMSE_facmoda
  mat_facmoda[2, i] <- as.integer(timer$getTimeElapsed("facmoda"))
}
mat_resu[1, 10] <- mean(mat_facmoda[1, ])
mat_resu[2, 10] <- mean(mat_facmoda[2, ])

# Num avant fac avec ordre par nombre de modalites
mat_numfac2 <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_numfac2) <- c("utilite", "temps")
for (i in 1:50) {
  vs_numfac2 <- c(14,20,2,21,22,16,17,1,15,13,19,10,11,12,18,5,3,7,4,8,6,9)
  timer$start("numfac2")
  syn_numfac2 <- syn(data, visit.sequence = vs_numfac2, seed = i)
  timer$stop("numfac2")
  
  pMSE_numfac2 <- utility.gen(syn_numfac2, data)$pMSE
  
  mat_numfac2[1, i] <- pMSE_numfac2
  mat_numfac2[2, i] <- as.integer(timer$getTimeElapsed("numfac2"))
}
mat_resu[1, 3] <- mean(mat_numfac2[1, ])
mat_resu[2, 3] <- mean(mat_numfac2[2, ])

# AFDM
mat_afdm <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_afdm) <- c("utilite", "temps")
for (i in 1:50) {
  vs_afdm <- c(6,3,7,5,9,2,21,11,12,1,19,20,22,8,4,16,17,13,15,10,18,14)
  timer$start("afdm")
  syn_afdm <- syn(data, visit.sequence = vs_afdm, seed = i)
  timer$stop("afdm")
  
  pMSE_afdm <- utility.gen(syn_afdm, data)$pMSE
  
  mat_afdm[1, i] <- pMSE_afdm
  mat_afdm[2, i] <- as.integer(timer$getTimeElapsed("afdm"))
}
mat_resu[1, 4] <- mean(mat_afdm[1, ])
mat_resu[2, 4] <- mean(mat_afdm[2, ])

# Aleatoire
mat_aleatoire <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_aleatoire) <- c("utilite", "temps")
for (i in 1:50) {
  vs_aleatoire <- sample(1:22)
  timer$start("aleatoire")
  syn_aleatoire <- syn(data, visit.sequence = vs_aleatoire, seed = i)
  timer$stop("aleatoire")
  
  pMSE_aleatoire <- utility.gen(syn_aleatoire, data)$pMSE
  
  mat_aleatoire[1, i] <- pMSE_aleatoire
  mat_aleatoire[2, i] <- as.integer(timer$getTimeElapsed("aleatoire"))
}
mat_resu[1, 5] <- mean(mat_aleatoire[1, ])
mat_resu[2, 5] <- mean(mat_aleatoire[2, ])

# Non correle
mat_noncorrele <- matrix(0, nrow = 2, ncol = 50)
row.names(mat_noncorrele) <- c("utilite", "temps")
for (i in 1:50) {
  vs_noncorrele <- c(12,9,21,10,3,17,6,13,16,5,18,4,20,11,22,14,15,7,8,1,19,2)
  timer$start("noncorrele")
  syn_noncorrele <- syn(data, visit.sequence = vs_noncorrele, seed = i)
  timer$stop("noncorrele")
  
  pMSE_noncorrele <- utility.gen(syn_noncorrele, data)$pMSE
  
  mat_noncorrele[1, i] <- pMSE_noncorrele
  mat_noncorrele[2, i] <- as.integer(timer$getTimeElapsed("noncorrele"))
}
mat_resu[1, 6] <- mean(mat_noncorrele[1, ])
mat_resu[2, 6] <- mean(mat_noncorrele[2, ])




