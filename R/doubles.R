#' Remove duplicates rows in a two dimensional table
#'
#'  The submitted tables can be matrices of numeric character or logical types.
#' @param x A matrix of numeric, character or logical type.
#' @return The submitted table with duplicated rows removed from.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' td0<-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1, 1,1,1),ncol=3,byrow=TRUE)
#' (doubles(td0))
#' td1<-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1),ncol=3,byrow=TRUE)
#' (doubles(td1))
#' td2<-matrix(c(1:3, 1:3,4:6,1:3),nrow=4,byrow=TRUE)
#' (doubles(td2))
#' td3<-matrix(c("d","e","f", rep(c("a","b","cc"),times=3),"g","h","i"),nrow=5,byrow=TRUE)
#' (doubles(td3))
#' td4<-matrix(as.logical(td1),nrow=5,byrow=TRUE)
#' (doubles(td4))
doubles<-function(x) {
  zi1<-array(x[1,],c(1,dim(x)[-1])) 
  zi2<-dotprod(zi1,t(x),g="&",f="==")  ## determining all positions of the tested line in x
  xr<-x[(c(1-zi2))*c(1:dim(x)[1]),,drop=FALSE] ## remove duplicates
#  res<- if (dim(xr)[1] <1) x else rbind(x[1,1:dim(x)[2]],doubles(xr)) ##  cycle until no duplicates in xr
 # test 
  res<- if (dim(xr)[1] < 1) x[1,] else rbind(x[1,1:dim(x)[2]],doubles(xr)) ##  cycle until no duplicates in xr
  # test
}