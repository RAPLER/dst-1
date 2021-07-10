#'Intersection of two tables of propositions
#' 
#' Function \code{inters} returns a table of the intersection between two (0,1) or boolean matrices or two vectors. The two matrices must have the same number of columns. The two vectors must be of the same length. This function generalizes the intersection of two subsets represented by boolean vectors to the intersection of two matrices of subsets. 
#' 
#' @param x A (0,1)-matrix or a boolean matrix of M rows by K columns, or a vector of length K.
#' @param y A (0,1)-matrix or a boolean matrix of N rows by K columns or a vector of length K.
#' @return The result is a (0,1)-table of dimensions (M x K) x N). In the case of vectors, the result is a (0,1)-table of dimensions (1 x K) x 1)
#' @author Claude Boivin, Stat.ASSQ
#' @examples 
#' mx<-matrix(c(0,1,0,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
#'  rownames(mx) <- nameRows(mx)
#' my<-matrix(c(0,0,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
#'  rownames(my) <- nameRows(my)
#' inters(mx,my)
#' b1 <- c(FALSE, TRUE, TRUE)
#' b2 <- c(TRUE, TRUE, FALSE)
#' names(b1) <- names(b2) <- c("c1","c2","c3")
#' inters(b1,b2)
#' x3<-matrix(c(1,1,0,1), ncol=2, dimnames=list(NULL, c("a","b")))
#' y3<-matrix(c(0,1,1,1), ncol=2, dimnames=list(NULL, c("a","b")))
#' inters(x3,y3)
#' x4 <-matrix(c(1,0,1,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
#' y4 <-matrix(c(1,0,0,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
#' inters(x4,y4)
#' @export
#' 
inters<-function(x, y) { 
  #
  # Local variables : I1, I2, N1, N2
  #
  # 2. checks
  #
  I1<-rbind(x) # transforms vectors in matrices
  I2<-rbind(y)
  if (ncol(I1) != ncol(I2)) {
    stop("Error in input arguments: check your input data.") 
  }
  # End checks
  #
  N1 <- outer(I1, I2, "*")
  N2 <- apply(N1, c(1,3), diag) # keep dim 1,3 and apply diag on the rest
  N12<-aperm(N2,c(2,1,3))
  return(N12)
}