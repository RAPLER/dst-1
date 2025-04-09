#' Impute commonality values based on a closure matrix
#' 
#' @details Impute commonality values based on a closure matrix
#'  
#' @param tty closure matrix
#' @param q1 named vector of commonality values
#' @param q2 named vector of commonality values
#' @param tt1 q1 support matrix
#' @param tt2 q2 support matrix
#' @return x a list with two elements \itemize{
#'  \item q1 new commonality vector
#'  \item q2 new commonality vector
#' }
#' @author Peiyuan Zhu
#' @import methods bit
#' @importFrom utils hashtab
#' @export
#' @examples
#' tt1 <- matrix(c(1,1,0,1,1,1), byrow=TRUE, nrow=2, dimnames=list(NULL,c("a","b","c")))
#' tt2 <- matrix(c(0,1,1,1,1,1), byrow=TRUE, nrow=2, dimnames=list(NULL,c("a","b","c")))
#' tty <- matrix(c(0,1,1,0,1,1,1,1,1,0,1,0), byrow=TRUE, nrow=4, dimnames=list(NULL,c("a","b","c")))
#' q1 <- c(1,0.9)
#' q2 <- c(1,0.8)
#' names(q1) <- nameRows(tt1)
#' names(q2) <- nameRows(tt2)
#' x <- imputeQQ(tty,tt1,tt2,q1,q2)
#' x$q1
#' x$q2
imputeQQ<-function(tty,tt1,tt2,q1,q2,use_tree=FALSE) {
  
  if(use_tree){
    
    tree1 <- buildTree(tt1,q1)
    tree2 <- buildTree(tt2,q2)
    
  }
  
  # Sort order of the joint
  sort_order <- order(rowSums(tty))
  tty <- tty[sort_order,]
  
  # Create hashtable
  # for commonality values that exist
  m1 <- hashtab()
  for (i in 1:nrow(tt1)) {
    m1[[as.bit(tt1[i,])]] <- q1[i]
  }
  
  m2 <- hashtab()
  for (i in 1:nrow(tt2)) {
    m2[[as.bit(tt2[i,])]] <- q2[i]
  }
  
  # Evaluate commonality values for q1, q2
  # Search for superset
  q1x <- rep(0, nrow(tty))
  q2x <- rep(0, nrow(tty))
  for (i in 1:nrow(tty)) {
    # Go through the entire list of subsets
    z <- as.bit(tty[i,])
    w1 <- m1[[z]]
    if (is.null(w1)) {
      # If commonality value doesn't exist
      if(use_tree) {
        
        ww1 <- superset(tree1,z)
        
      } else {
        for (j in 1:nrow(tt1)) {
          if (all((tt1[j,] - tty[i,] >= 0))) {
            ww1 <- q1[j]
            break
          }
        }
      }
      
      q1x[i] <- unname(ww1)
      v <- t(as.logical(z))
      colnames(v) <- colnames(tt1)
      names(q1x)[i] <- nameRows(v)
      
    } else {
      # If commonality value exists
      q1x[i] <- w1
      names(q1x)[i] <- names(w1)
    }
    
    w2 <- m2[[z]]
    if (is.null(w2)) {
      # If commonality value doesn't exist
      if(use_tree) {
        
        ww2 <- superset(tree2,z)
        
      } else {
        for (j in 1:nrow(tt2)) {
          if (all((tt2[j,] - tty[i,]) >= 0)) {
            ww2 <- q2[j]
            break
          }
        }
      }
      q2x[i] <- unname(ww2)
      v <- t(as.logical(z))
      colnames(v) <- colnames(tt1)
      names(q2x)[i] <- nameRows(v)
      
    } else {
      # If commonality value exists
      q2x[i] <- w2
      names(q2x)[i] <- names(w2)
    }
  }
  
  q1 <- q1x
  q2 <- q2x
  
  x <- list("q1"=q1,"q2"=q2)
  
  return(x)
}