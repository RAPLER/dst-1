#' Intersect two vectors of ssnames
#' 
#' @param zx a vector of ssnames from one bca
#' @param yz a vector of ssnames from another bca
#' @return ssnames in the intersection of the two bcas
#' @author Peiyuan Zhu
#' @export
#' @examples
#' y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1) 
#' y2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1)
#' intersBySSName(y1$ssnames[[1]], y2$ssnames[[2]])
intersBySSName<-function(zx,yz) {
  zz <- intersect(zx,yz)
  if (length(zz)==0)
    return("Empty")
  else
    return(zz)
}
