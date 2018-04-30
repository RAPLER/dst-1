#' Obtain dimensions of an array or length of a vector with a single command
#' 
#' \code{shape} returns sizes of each dimension of given array or the length of a given vector. The function is taken from the project APL in R (\url{https://rpubs.com/deleeuw/158476}). 
#' @aliases aplShape
#' @export
#' @param a An array or a vector.
#' @return The dimensions of the array \code{a} or the length of the vector \code{a}.
#' @examples
#' shape(array(c(1:6), c(2,3)))
#' shape(c("a", "b"))
#' @author Claude Boivin, Stat.ASSQ.
#' 
#' @references \itemize{
#' \item Jan de Leeuw and Masanao Yajima (March 07, 2016) \emph{APL in R (Version 009)}, Source code. \url{https://rpubs.com/deleeuw/158476}
#'   \item G. Helzer. (1989): \emph{An Encyclopedia of APL}, second edition, I-APL LTD, St. Albans, G.B.
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  }
shape <- function(a) 
  {
    if (is.vector(a)) return(length(a))
    return(dim(a))
  }

