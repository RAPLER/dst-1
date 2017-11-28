#' Obtain dimensions of an array or length of a vector with a single command
#' 
#' \code{shape} returns sizes of each dimensions of given array or the length of a given vector.
#'
#' @aliases aplShape
#' @param a An array or a vector.
#' @return The dimensions of the array \code{a} or the length of the vector \code{a}.
#' @examples
#' shape(array(c(1:6), c(2,3)))
#' shape(c("a", "b"))
#' @author Original author of the aplShape function: Jan de Leeuw 
#'       \url{http://www.codecollector.net/view/8A8D9395-0F66-4706-A23E-C588151E8423-95744-0000429BCF33A153}.
#' @seealso Jan de Leeuw and Masanao Yajima: \url{https://rpubs.com/deleeuw/158476}.
#' @references \itemize{
#'   \item G. Helzer. (1989): \emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  }
#'  @export
shape <- function(a) 
  {
    if (is.vector(a)) return(length(a))
    return(dim(a))
  }

