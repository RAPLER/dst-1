#' simple support bca
#' 
#' @details simple support bca
#'  
#' @param x 
#' @param y 
#' @param a 
#' @param y0 
#' @param flip 
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