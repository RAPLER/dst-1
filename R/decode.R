#' Find the value in base 10 of a number coded in another base
#' 
#' The \code{aplDecode} function of the project APL in R (\url{https://rpubs.com/deleeuw/158476}) has been adapted to follow the standard implementation of the APL \code{decode} function. 
#' @aliases aplDecode
#' @param base A scalar or a numeric vector which describes the number system in which the data is coded.
#' @param ind The value to decode represented by a numeric vector in the \code{base} system.
#' @return A scalar representing the conversion of the coded number \code{ind} to its decimal representation.
#' @details If the base value is a number system, namely base 2, we need only to enter a scalar, which is treated to match the length of the expression to decode. 
#' If the base value is a number system, e.g. base 2, we need only to enter it as a scalar, which is then processed to match the length of the expression to decode. If \code{length(ind)}  is less than \code{length(base)}, zeroes are added to the left of the vector \code{ind}  to match the length of the two vectors. And vice-versa. 

#' @author Claude Boivin, Stat.ASSQ. 
#' @export
#' @references \itemize{
#' \item Jan de Leeuw and Masanao Yajima (March 07, 2016) \emph{APL in R (Version 009)}, Source code. \url{https://rpubs.com/deleeuw/158476}
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New York.
#'  \item  APL 68000 Level II language manual. MicroAPL Ltd. 1990.
#'  }
#'  
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