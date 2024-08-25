filtres_puf <- function(data) {
  # HEFFEMP et ACTEU = 2 ou 3
      # Une personne ayant un nombre d'heures effectivement travaillées au cours
      # de la semaine de référence ne peut pas être au chômage ou inactive (ACTEU = 2 ou 3).
  filtre1 <- sum(table(data$HEFFEMP, data$ACTEU)[2:dim(table(data$HEFFEMP, data$ACTEU))[1], 2:3])
  
  # TXTPPRED = 1 et ACTEU = TPPRED
      # Si une personne est à temps complet (TPPRED = 1) elle ne peut pas avoir 
      # un taux de temps partiel autre que NA.
  filtre2 <- sum(table(data$TXTPPRED, data$TPPRED)[1:dim(table(data$TXTPPRED, data$TPPRED))[1]-1, 1])
  
  # TRAREF = 1 et ACTEU = 2 ou 3 
      # Si une personnes a travaillé au moins une heure pendant la semaine de référence (TRAREF = 1)
      # elle ne peut être au chômage ou inactive (ACTEU = 2 ou 3).
  
  filtre3 <- sum(table(data$TRAREF, data$ACTEU)[1, 2:3])
  
  # TEMP = 1 et ACTEU = 2 ou 3
      # Une personne ayant un emploi pendant la semaine de référence (TEMP = 1) ne peut être
      # au chômage (ACTEU = 2) ou inactive (ACTEU = 3).
  
  filtre4 <- sum(table(data$TEMP, data$ACTEU)[1, 2:3])
  
  # STATUT = 1 ou 2 et ACTEU = 2 ou 3
      # Si une personne est indépendante (STATUT = 1) ou salariée (STATUT = 2) elle ne peut être
      # au chômage (ACTEU = 2) ou inactive (ACTEU = 3).
  
  filtre5 <- sum(table(data$STATUT, data$ACTEU)[1:2, 2:3])
  
  # STATUTDET et ACTEU = 2 ou 3
      # Même raison qu'au dessus.
  
  filtre6 <- sum(table(data$STATUTDET, data$ACTEU)[1:dim(table(data$STATUTDET, data$ACTEU))[1]-1, 2:3])
  
  # STC et ACTEU = 2 ou 3
      # Même raison qu'au dessus.
  
  filtre7 <- sum(table(data$STC, data$ACTEU)[1:dim(table(data$STC, data$ACTEU))[1]-1, 2:3])
  
  # SALTYP et ACTEU = 2 ou 3
      # Une personne au chômage (ACTEU = 2) ou inactive (ACTEU = 3) doit répondre
      # NA quand on lui demande la nature du contrat pour son emploi principal (SALTYP).
  
  filtre8 <- sum(table(data$SALTYP, data$ACTEU)[1:dim(table(data$SALTYP, data$ACTEU))[1]-1, 2:3])
  
  # TPPRED = 1 et RAISTP
      # Si une personne est en temps complet (TPPRED = 1) elle ne peut donner
      # de raison principale de temps partiel (RAISTP).
  
  filtre9 <- sum(table(data$TPPRED, data$RAISTP)[1, 1:dim(table(data$TPPRED, data$RAISTP))[2]-1])
  
  # PUB3FP et ACTEU = 2 ou 3
      # Une personne au chômage (ACTEU = 2) ou inactive (ACTEU = 3) ne peut donner
      # le statut public ou privé de l'employeur (PUB3FP).
  
  filtre10 <- sum(table(data$PUB3FP, data$ACTEU)[1:dim(table(data$PUB3FP, data$ACTEU))[1]-1, 2:3])
  
  # ANCSSEMP = 1 et ACTEU = 1
      # Une personne en emploi (ACTEU = 1) doit répondre NA lorsqu'on lui demande
      # l'ancienneté sans emploi (ANCSSEMP).
  
  filtre11 <- sum(table(data$ANCSSEMP, data$ACTEU)[1:dim(table(data$ANCSSEMP, data$ACTEU))[1]-1, 1])
  
  # CHPUB et ACTEU = 2 ou 3
      # Une personne au chômage (ACTEU = 2) ou inactive (ACTEU = 3) ne peut donner
      # la nature l'employeur (CHPUB).
  
  filtre12 <- sum(table(data$CHPUB, data$ACTEU)[1:dim(table(data$CHPUB, data$ACTEU))[1]-1, 2:3])
  
  # CL_EMPLOI et ACTEU = 2 ou 3
      # Une personne au chômage (ACTEU = 2) ou inactive (ACTEU = 3) ne peut donner
      # la classe d'emploi de l'emploi principal (CL_EMPLOI).
  
  filtre13 <- sum(table(data$CL_EMPLOI, data$ACTEU)[3:dim(table(data$CL_EMPLOI, data$ACTEU))[1], 2:3]) +
    sum(table(data$CL_EMPLOI, data$ACTEU)[1, 2:3])
  
  cat("Individus ne respectant pas les filtres : \n",
      "Filtre1 : ", filtre1, "\n",
      "Filtre2 : ", filtre2, "\n",
      "Filtre3 : ", filtre3, "\n",
      "Filtre4 : ", filtre4, "\n",
      "Filtre5 : ", filtre5, "\n",
      "Filtre6 : ", filtre6, "\n",
      "Filtre7 : ", filtre7, "\n",
      "Filtre8 : ", filtre8, "\n",
      "Filtre9 : ", filtre9, "\n",
      "Filtre10 : ", filtre10, "\n",
      "Filtre11 : ", filtre11, "\n",
      "Filtre12 : ", filtre12, "\n",
      "Filtre13 : ", filtre13, "\n")
}










