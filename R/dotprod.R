#' Generalized inner product of two matrices
#' 
#' The generalized inner product of two matrices combines two operators in the same manner than the classical inner product defined for the multiplication of two matrices. The number of rows of the second matrix must be equal the number of columns of the first matrix.
#' 
#' @param x A matrix of M rows by K columns.
#' @param y A matrix of K rows by N columns.
#' @param g Any operator: +, -, *, /, &, |, ==, <=, paste etc. 
#' @param f Any operator: +, -, *, /, &, |, ==, <=, paste etc. 
#' @return The result of the generalized inner product is returned.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' print("Standard matrix product")
#' x <- y <- matrix(c(1:6), nrow = 2, byrow = TRUE)
#' dotprod(x, t(y), g = "+", f = "*") ## same as x %*% t(y)
#' print("Find some data x2 in the rows of a larger matrix y2")
#' x2 <- matrix(c(1,0,0,1,1,1), nrow = 2, byrow = TRUE)
#' y2 <- matrix(c(1,0,0,0,1,0,1,1,0,0,1,1,1,1,1), nrow = 5, byrow = TRUE)
#' (1:nrow(y2)) * dotprod(x2, t(y2), g = "&", f = "==")
#' 
#' print("Find some names in a long list")
#' team_names <- matrix(c("Patrick", "Dole", "Amanda", "Dole", "Robert", "Calvin", "Alvina", "Klein", "Robert", "Gariepy", "Nellie", "Arcand"), ncol = 2, byrow = TRUE)
#' colnames(team_names) <- c("First_name", "Last_name")
#' print("Where in the list are the person with first name Robert and where are the Doles?")
#' BobandDoles <- matrix(c("Robert", "", "", "Dole"), ncol = 2, byrow = TRUE)
#' dotprod(team_names, t(BobandDoles),g="|",f="==") * (1:nrow(team_names))
dotprod<-function(x,y,g,f){
  if (ncol(x) !=nrow(y)) {
    stop("nb of columns of first matrix not equal to nb rows second matrix.")
  }
  ff<-match.fun(f); gg<-match.fun(g)
  resul<-matrix(FALSE,nrow(x),ncol(y))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(y)){
      temp<-ff(x[i,1],y[1,j])
      # if the first table has more than one column
      if (ncol(x) > 1) {
        for (k in 2:ncol(x)) {
          temp<-gg(temp,ff(x[i,k],y[k,j]))
        }
      }
      resul[i,j]=temp
    }
  }
 resul
}
