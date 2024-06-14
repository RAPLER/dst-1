#' Construct a tt matrix of focal elements from a list of tt matrices
#' 
#' @param TT List of tt matrices
#' @param valuenames Vector of valuenames
#' @return tt A corresponding logical description matrix of the focal elements
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' TT<-list(matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#'  tt = matrix(c(1,0,0,1,1,1),nrow = 2, byrow = TRUE))
#' ttmatrixFromTT(TT)
ttmatrixFromTT <- function(TT,valuenames) {
  # Check that the input valuenames is a vector
  if (is.vector(valuenames) == FALSE) {
    stop("Input valuenames must be a vector")
  }
  tt<-Reduce(function(x,y) { res<-inters(x,y); res<-res[!duplicated(res),]; return(res)},TT)
  colnames(tt) <- valuenames
  rownames(tt) <- nameRows(tt)
  sort_order <-order(apply(tt,1,sum))
  tt<-tt[sort_order,]
  return(tt)
}