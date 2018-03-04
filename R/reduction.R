#' Summary of a vector for any operator.
#' 
#' This utility function is used to obtain a summary of a vector of data for many operators. This code has been adapted from the \code{aplRDV} R function of Jan de Leeuw.
#' @aliases aplRDV
#' @param x A vector of numbers or strings.
#' @param f The operator. Must be compatible with the type of vector (numeric or character)
#' @return The result of applying the chosen operator to all the elements of the vector is an object of length 1.
#' @author \itemize{
#'  \item Claude Boivin, Stat.ASSQ. 
#'  \item Original author of the aptRDV function: Jan de Leeuw 
#'       \url{http://www.codecollector.net/view/8A8D9395-0F66-4706-A23E-C588151E8423-95744-0000429BCF33A153}.
#'       }
#' @seealso Jan de Leeuw and Masanao Yajima: \url{https://rpubs.com/deleeuw/158476}.
#' @references \itemize{
#'   \item G. Helzer. (1989): \emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  }
#'  @export
#'  @examples
#'  reduction(c(1,2,3,4), f="-")
#'  reduction(c(1,0,1,1,0), f="|")
#'  reduction(c("a", "b", "c"), f="paste")
#'  
reduction<-function(x,f="+") {
  if (length(x) == 0) return(x)
  s<-x[1]
  if (length(x) == 1) return(s)
  for (i in 2:length(x))
    s<-match.fun(f)(s,x[i])
  return(s)
}