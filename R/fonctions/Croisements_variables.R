croisements <- function(dataorg, datasyn, variable1, variable2) {
  liste <- list()
  liste[[1]] <- table(dataorg[[variable1]], dataorg[[variable2]], useNA = "always")
  liste[[2]] <- table(datasyn[[variable1]], datasyn[[variable2]], useNA = "always")
  names(liste) <- c("Original", "Synthétique")
  return(liste)
}