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
  
  qq <- rep(0,nrow(x_c))
  
  pb <- progress_bar$new(
    format = "  computing commonality [:bar] :percent eta: :eta",
    total = nrow(x_c), clear = FALSE, width= 100)
  
  # TODO: make this faster
  print("compute commonality starts")
  start.time <- Sys.time()
  for(i in 1:nrow(x_c)) {
    pb$tick()
    qq[i] <- (1 - a) ** sum(sapply(seq_len(nrow(x)), function(j) { 
      !all(x[j, ] - x_c[i, ] >= 0) }))
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute commonality finishes within")
  print(time.taken)
  
  print("compute mobius inversion starts")
  start.time <- Sys.time()
  m <- mFromQQ(qq,method="emt-m",tt=x_c,use_pb=TRUE,tree_type=tree_type)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute mobius inversion finishes within")
  print(time.taken)
  
  z <- list("tt"=x_c, "qq"=qq, "m"=m)
  
  return(z)
}