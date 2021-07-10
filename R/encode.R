#' Convert a value to its representation in another chosen base
#' 
#' The \code{aplEncode} function of the project APL in R (\url{https://rpubs.com/deleeuw/158476}) has been adapted to follow the standard implementation of the APL \code{encode} function. 
#' @aliases aplEncode
#' @param base A numeric vector which describes the number system in which we want to recode the data.
#' @param ind The value to convert represented by a number or a numeric vector.
#' @return A vector or a matrix of the data converted.
#' @author Claude Boivin, Stat.ASSQ. 
#' @references \itemize{
#' \item Jan de Leeuw and Masanao Yajima (March 07, 2016) \emph{APL in R (Version 009)}, Source code. \url{https://rpubs.com/deleeuw/158476}
#'  \item  L. Gilman and A. J. Rose.(1974): \emph{APL an Interactive Approach}, Second Edition, John Wiley, New York.
#'  \item  APL 68000 Level II language manual. MicroAPL Ltd. 1990.
#'  }
#'  
#' @examples 
#' encode(c(2,2,2,2), 11)  # find the base 2 representation of number 11
#' encode(c(365,24,60), 2997) # convert 2997 minutes to days-hrs-min.
#' @export
#' 
encode<-function(base, ind) { 
  #
  # Local variables: s, j
  #
r<-rep(0,length(base))
		s<-ind[1] 
		for (j in length(base):1) {
			r[j]<-s-base[j]*s%/%base[j]
			s<-(s-r[j])/base[j]
		}
 if (shape(ind) ==1) return(r)
		else return(cbind(r,encode(base, ind<-ind[-1])))
return(r)
}
	