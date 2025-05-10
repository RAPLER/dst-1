#' Fast combination of multiple simple support functions
#' 
#' @details This function handles the case of combining several simple support functions having the same mass. The simple support functions are arranged row by row in a binary array \code{x]. The number of rows of the array is equal to the number of support functions to be combined.The support functions can be defined as is or by their complement when it's faster to code. A column matrix \code{y} of TRUE/FALSE values is used to identify the rows of the array that need to be inverted.
#'  
#' @param x A binary matrix of simple support functions define on a same frame of discernment (Fod).
#' @param y A column matrix \code{y} of TRUE/FALSE values.
#' @param a The mass value allotted to each simple support function.
#' @param y0 A value used to check which rows of the matrix \code{x} are to be inverted. Set at 0 (FALSE).
#' @param flip Parameter used when some rows of the table \code{x} need to be reversed. The default value is TRUE (check rows).
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