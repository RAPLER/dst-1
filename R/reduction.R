#' Summary of a vector for any operator.
#' 
#' This utility function is used to obtain a summary of a vector of data for many operators. The function is taken from the project APL in R (\url{https://rpubs.com/deleeuw/158476}). 
#' @aliases aplRDV
#' @param x A vector of numbers or a character string.
#' @param f The operator. Must be compatible with the type of input vector (numeric or character)
#' @return The result of applying the chosen operator to all the elements of the vector is an object of length 1.
#' 
#' @examples
#'  reduction(c(1,2,3,4), f = "-")
#'  reduction(c(1,0,1,1,0), f = "|")
#'  reduction(c("a", "b", "c"), f = "paste")
#'  
#' @export
#' @author Claude Boivin
#' 
#' @references \itemize{
#' \item Jan de Leeuw and Masanao Yajima (March 07, 2016) \emph{APL in R (Version 009)}, Source code. \url{https://rpubs.com/deleeuw/158476}
#' \item G. Helzer. (1989): \emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
#' \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  }
#'  
reduction<-function(x,f) {
  #
  # Local variables: None
  # Functions calls: None
  #
  if (length(x) == 0) return(x)
  s<-x[1]
  if (length(x) == 1) return(s)
  for (i in 2:length(x))
    s<-match.fun(f)(s,x[i])
  return(s)
}