superBca<-function(x,y,a,y0=0) {
  x <- methods::as(x, "RsparseMatrix")
  x[y==y0,] <- 1 - x[y==y0,]
  
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
    format = "  computing closure [:bar] :percent eta: :eta",
    total = nrow(x_c), clear = FALSE, width= 100)
  
  print("compute closure starts")
  start.time <- Sys.time()
  for(i in 1:nrow(x_c)) {
    pb$tick()
    qq[i] <- (1 - a) ** sum(sapply(seq_len(nrow(x)), function(j) { 
      !all(x[j, ] - x_c[i, ] >= 0) }))
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute closure finishes within")
  print(time.taken)
  
  print("compute mobius inversion starts")
  start.time <- Sys.time()
  m <- mFromQQ(qq,method="emt-m",tt=x_c,use_pb=TRUE)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print("compute mobius inversion finishes within")
  print(time.taken)
  
  return(list("tt"=x_c, "qq"=qq, "m"=m))
}