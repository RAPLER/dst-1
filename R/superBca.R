#' Fast combination of multiple simple support functions
#' 
#' @details This function handles the case of combining several simple support functions defined on a very large frame of discernment (Fod). The simple support functions are arranged row by row in a binary array \code{x}. The number of rows of the array is equal to the number of support functions to be combined.The support functions can be defined as is or by their complement when it's faster to code. A column matrix \code{y} of TRUE/FALSE values is used to identify the rows of the array that need to be inverted.
#'  
#' @param x A binary matrix of simple support functions define on a same frame of discernment (Fod).
#' @param y A column matrix \code{y} of TRUE/FALSE values.
#' @param a The mass value allotted to each simple support function. All support functions have the same mass. 
#' @param y0 A value used to check which rows of the matrix \code{x} are to be inverted. Set at 0 (FALSE).
#' @param flip Parameter used when some rows of the table \code{x} need to be reversed. The default value is TRUE (check rows).
#' @return z An object of class \code{bcaspec} called a bca for "basic chance assignment"
#' @author Peiyuan Zhu
#' @export
#' @examples
#' x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#'    m = c(0.9,0.1), cnames = c("yes", "no"),
#'    varnames = "x", idvar = 1)
#' x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE),
#'    m = c(0.9,0.1), cnames = c("yes", "no"), 
#'    varnames = "x", idvar = 1)
#' x <- rbind(x1$tt[1,], x2$tt[1,])
#' rownames(x) <- nameRows(x)
#' y <- matrix(c(TRUE, TRUE), ncol = 1 )
#' a <- 0.9
#' z <- superBca(x, y, a, tree_type="single")
#' 
superBca<-function(x,y,a,y0=0,flip=TRUE,tree_type="single", varnames = NULL, valuenames = NULL, idvar = 1) {
  x <- methods::as(x, "RsparseMatrix")
  if(flip) x[y==y0,] <- 1 - x[y==y0,]
  
  print("compute closure starts")
  start.time <- Sys.time()
  
  x_c <- closureSparse(x, FALSE, TRUE)
  # Check column names
  if (is.null(colnames(x))) {    
    colnames(x_c) <- colnames(x_c, do.NULL = FALSE, prefix = "c") 
  }
  else {
    colnames(x_c) <- colnames(x)
    rownames(x_c) <- nameRows(x_c)
  }
  #
  
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
  
  # Output result
  # z <- list("tt"=x_c, "qq"=qq, "m"=m)
 
  # Build specification matrix spec
  spec <- cbind((1:nrow(x_c)), m)
  colnames(spec) <- c("specnb", "mass")
  rownames(spec) <- rownames(x_c)
  #
  infovar <- matrix(c(idvar, ncol(x_c)), ncol = 2)
  if (is.null(valuenames) | missing(valuenames)) {
    valuenames <- split(colnames(x_c), rep(paste(rep("v",length(idvar)),c(1:length(idvar)),sep=""), infovar[,2]))
  }
  if (is.null(varnames)) {
    varnames <- names(valuenames)
  } 
  z <-list(con = NULL, "tt"=x_c, "qq"=qq, "m"=m,  method = "emt-m", spec = spec , infovar = infovar, varnames = varnames, valuenames = valuenames, ssnames = NULL, inforel = NULL) 
  # end test
  #
  class(z) <- append(class(z), "bcaspec")
  
  return(z)
}