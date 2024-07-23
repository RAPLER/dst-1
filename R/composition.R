#' Compose two relation
#' 
#' Calculate Schöder's composition of two relations
#' 
#' @param R first relation from set A to set B to be combined, encoded by a binary matrix
#' @param S second relation from set B to set C to be combined, encoded by a binary matrix
#' @return The Schöder-composed relation \deqn{J = \max_{j\in B}(\min(R_{i,j}, S_{j,k}))} indicating whether there exists an intermediate element connecting the two elements from A and B
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' R1 <- matrix(c(1,1,0,0,1,1,0,0,1), nrow=3, byrow = TRUE)
#' composition(R1,R1)
composition<-function(R,S){
  if (!is.matrix(R) || !is.matrix(S)) {
    stop("R, S must be of type matrix")
  }
  if (!all(R %in% c(0,1)) || !all(S %in% c(0,1))) {
    stop("R, S must be binary")
  }
  J<-dotprod(R,S,max,min)
  return(J)
}
