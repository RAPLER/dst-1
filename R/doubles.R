#' Remove duplicate rows in a two-dimensional table. 
#' 
#' Recursive function.
#' @param x A matrix of numeric, character or logical type.
#' @return The submitted matrix with duplicated rows removed from.
#' @author Claude Boivin
#' @export
#' @examples 
#' td0 <- matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1, 1,1,1),ncol = 3,byrow = TRUE)
#' (doubles(td0))
#' td1 <- matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1),ncol = 3,byrow = TRUE)
#' (doubles(td1))
#' td2 <- matrix(c(1:3, 1:3,4:6,1:3),nrow = 4,byrow = TRUE)
#' (doubles(td2))
#' td3 <- matrix(c("d","e","f", rep(c("a","b","cc"),times = 3),"g","h","i"),nrow = 5,byrow = TRUE)
#' (doubles(td3))
#' td4 <- matrix(as.logical(td1),nrow = 5,byrow = TRUE)
#' (doubles(td4))
doubles<-function(x) {
  #
  # Recursive
  # Local variables: zi1, zi2,  xr
  # Functions calls: dotprod
  #
  # 1. checks
  #
  if (is.matrix(x) == FALSE) {
    stop("Input is not a matrix.")
  }
  #
  # 2. Treatment
  #
  zi1<-array(x[1,],c(1,dim(x)[-1])) 
  zi2<-dotprod(zi1,t(x),g="&",f="==")  ## determining all positions of the tested line in x
  xr<-x[(c(1-zi2))*c(1:dim(x)[1]),,drop=FALSE] ## remove duplicates
  res<- if (dim(xr)[1] < 1) x[1,] else rbind(x[1,1:dim(x)[2]],doubles(xr)) ##  cycle until no duplicates in xr
  #
  # 3. Result
  #
  # convert to matrix if result is vector
  if (is.matrix(res) == FALSE) {
    res <- t(as.matrix(res))
  }
  res
}