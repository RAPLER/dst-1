#' Obtain the summary of a vector for any operator.
#' 
#' @export
#' @aliases aplRDV
#' \code{reduction} returns...
#' @param x The data...
#' @param f The operator.
#' @author Original author of the aptRDV function: Jan de Leeuw 
#'       \url{http://www.codecollector.net/view/8A8D9395-0F66-4706-A23E-C588151E8423-95744-0000429BCF33A153}.
#' @seealso Jan de Leeuw and Masanao Yajima: \url{https://rpubs.com/deleeuw/158476}.
#' @references \itemize{
#'   \item G. Helzer. (1989): \emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  }
#'  @examples
#'  
reduction<-function(x,f="+") {
  if (length(x) == 0) return(x)
  s<-x[1]
  if (length(x) == 1) return(s)
  for (i in 2:length(x))
    s<-match.fun(f)(s,x[i])
  return(s)
}