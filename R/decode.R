#' Find the value in base 10 of a number coded in another number system
#' 
#' This utility function is used to find the value in base 10 of a number represented in another number system. This code has been adapted from the \code{aplDecode} R function of Jan de Leeuw. It follows the standard decode implementation of the APL language.
#' @aliases aplDecode
#' @param base A scalar or a numeric vector which describe the number system in which the data is coded.
#' @param ind The value to decode represented by a numeric vector in the \code{base} system.
#' @return A scalar representing the conversion of the coded number \code{ind} to its decimal representation.
#' @details If the base value is a number system, namely base 2, we need only to enter a scalar, which is treated to match the length of the expression to decode. 
#' if \code{length(ind)} is less than \code{length(base)}, \code{0} values are added to the left of the vector \code{ind} to match the length of the two vectors. And vice-versa. 
#' @author \itemize{
#'  \item Claude Boivin, Stat.ASSQ. 
#'  \item Author of the aplDecode function: Jan de Leeuw 
#'       \url{http://www.codecollector.net/view/8A8D9395-0F66-4706-A23E-C588151E8423-95744-0000429BCF33A153}.
#'  }
#' @seealso Jan de Leeuw and Masanao Yajima: \url{https://rpubs.com/deleeuw/158476}.
#' @export
#' @references \itemize{
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New-York.
#'  \item  APL 68000 Level II language manual. MicroAPL Ltd. 1990.
#'  }
#' @examples 
#' decode(c(2,2,2,2), c(1,0,1,1)) #   Find the base 10 value of the base 2 number 1011.
#' decode(2, c(1,0,1,1))  # left argument is extended to vector c(2,2,2,2)
#' decode(c(365,24,60), c(2,1,57)) # transform 2 days 1 h 57 min in minutes
#' decode(c(365,24,60), c(1,57))   # right vector extended
#' decode(c(24,60), c(2,1,57))     # left vector extended
#' decode(1.5, c(1,2,3)) # polynomial 1*x^2 +2*x +3 evaluated at x=1.5
decode<-function(base, ind) { 
	if (length(base) == 1) {
	   base<-array(base,shape(ind)) 
	}
  if (length(base) < length(ind)) {
    base<-c(rep(0, length(ind)-length(base)), base)
  }
  if (length(ind) < length(base)) {
    ind <- c(rep(0, length(base)-length(ind)), ind)
  }
	  base<-base[shape(base):1]	
    b1<-c(1,cumprod(base[-length(base)]))
 	  b<-b1[shape(b1):1]	
    return(sum(b*ind))
}