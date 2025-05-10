#' Fast combination of many support functions
#' 
#' @details The case of many support functions with the same mass.
#'  
#' @param x The binary matrix of support functions
#' @param y A logical column matrix which row of the matrix x must be inverted
#' @param a The mass value of the simple support functions
#' @param y0 The value used to check for the rows of x to invert.
#' @param flip TRUE by default. Function will check for the presence of rows to invert.
#' @return z a list with three elements \itemize{
#'  \item tt 
#'  \item qq 
#'  \item z
#' }
#' @author Peiyuan Zhu
#' @export
#' @examples
#' 1
superBca<-function(x,y,a,y0=0,flip=TRUE,tree_type="single") {
  x <- methods::as(x, "RsparseMatrix")
  if(flip) x[y==y0,] <- 1 - x[y==y0,]
  
  print("compute closure starts")
  start.time <- Sys.time()
  
  x_c <- closureSparse(x, FALSE, TRUE)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute closure finishes within")
  print(time.taken)
  
  x_c <- rbind(x_c, methods::as(t(rep(1,ncol(x_c))), "RsparseMatrix"))
  
  print("compute commonality starts")
  start.time <- Sys.time()
  
  qq<-commSparse(x,x_c,a,TRUE)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute commonality finishes within")
  print(time.taken)
  
  m <- mFromQQ(qq,method="emt-m",tt=x_c,use_pb=TRUE,tree_type=tree_type)
  
  z <- list("tt"=x_c, "qq"=qq, "m"=m)
  
  return(z)
}