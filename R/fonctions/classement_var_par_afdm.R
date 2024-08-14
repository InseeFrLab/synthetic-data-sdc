#' Ordonne les variables en fonction de leur importance dans la construction
#' des différents axes factoriels d'une AFDM. Utile 
#'
#' @param data 
#' @param all_num booléen indiquant si toutes les variables sont numériques 
#' Si FALSE (par défaut), une AFDM est réalisée, sinon c'est une ACP.
#'
#' @return 
#' @export
#'
#' @examples
classement_var <- function(data, all_num = FALSE) {
  if(all_num){
    data.fact <- PCA(data, ncp = 1000, graph = FALSE)
  }else{
    data.fact <- FAMD(data, ncp = 1000, graph = FALSE)
  }
  eig.val <- data.fact$eig
  matrice <- matrix(0, nrow = 1, ncol = ncol(data))
  for (i in 1:ncol(data)) {
    var <- 0
    for (j in seq_along(eig.val[, 2])) {
      var <- var + eig.val[, 2][j] * data.fact$var$contrib[names(data)[i], j]
    }
    matrice[1, i] <- var
  }
  colnames(matrice) <- names(data)
  matrice <- matrice[, order(matrice, decreasing = TRUE)]
  ordre <- names(matrice)
  
  
  return(list(matrice = matrice, ordre = ordre))
}