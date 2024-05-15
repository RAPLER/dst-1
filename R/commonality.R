# Compute qq from tt
#' @details
#' @param
#' @return
#' @author
#' @import 
#' @importClassesFrom 
#' @export
#' @examples 
#' 
commonality <- function(tt,m){
  f <- function(x) {
    q <- 0
    for (i in 1:nrow(tt)) {
      if (all(tt[i,] - x >= 0)) {
        q <- q + m[i]
      }
    }
    return(q)
  }
  return(f)
}