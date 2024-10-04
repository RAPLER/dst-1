#' Construct a mass vector from qq function and ttmatrix of focal elements recursively.
#' 
#' @param qq Commonality function
#' @param n Frame dimension
#' @param method = NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt")
#' @return m A corresponding mass vector
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' mFromQQ(x$qq, x$tt)
mFromQQRecursive <- function(qq,n,method = NULL) {
  # Obtain tt matrix from commonality function
  #
  # 1. Check that the input qq is a function
  if (is.function(qq) == FALSE) {
    stop("Input qq must be a function.")
  }
  
  # 2. Check that the input method is not NULL
  if (is.null(method) == TRUE) {
    stop("Input method must be either fmt or emt")
  }
  
  if (method=="fmt") {
    # Fast Mobius Transform
    m_seq <- rep(0, 2**n)
    for (i in 1:length(m_seq)) {
      w <- encode(rep(2, n), i - 1)
      m_seq[i] <- qq(w)
    }
    
    for (i in 1:n) {
      x <- rep(1,n)
      x[i] <- 0
      for (j in 1:2**n) {
        y <- encode(rep(2, n), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, n), z)
        if (!all(z==y)) {
          m_seq[w + 1] <- m_seq[w + 1] - m_seq[j]
        }
      }
    }
    
    return(m_seq)
  } else if (method == "emt") {
    # Load tt, qq
    
    W2 <- 0
    MACC <- 0
    #
    # Use Efficient Zeta Transform
    #
    # Step 0.1 Insert complements of W2 into W2
    W2c <- 1-W2
    
    rownames(W2c) <- nameRows(1-W2)
    W21 <- rbind(W2,W2c)
    
    # Step 0.1.1 insert closure elements
    W2x <- W21
    for (i in 1:nrow(W21)) {
      for (j in i:nrow(W21)) {
        z <- pmax(W21[i,],W21[j,])
        x <- which(apply(W2x, 1, function(x) return(all(x == z))))
        if (length(x) == 0) {
          z <- t(as.matrix(z))
          rownames(z) <- nameRows(z)
          W2x <- rbind(W2x,z)
        }
        
        z <- pmin(W21[i,],W21[j,])
        x <- which(apply(W2x, 1, function(x) return(all(x == z))))
        if (length(x) == 0) {
          z <- t(as.matrix(z))
          rownames(z) <- nameRows(z)
          W2x <- rbind(W2x,z)
        }
      }
    }
    W21 <- W2x
    
    MACCc <- rep(0,nrow(W21)-nrow(W2))
    names(MACCc) <- rownames(W21)[(nrow(W2)+1):nrow(W21)]
    MACC1 <- c(MACC,MACCc)
    
    # Step 0.2 Remove duplicates
    MACC2 <- MACC1[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 1.0: Sort W2, MACC
    sort_order <- order(apply(W22,1,sum))
    W23 <- W22[sort_order,]
    MACC3 <- MACC2[sort_order]
    
    # Step 1.1: Find all join-irreducible elements by checking if it's a union of any two elements less than that
    rho <- rowSums(W23)
    jir <- rep(0,nrow(W23))
    l <- 1
    for (i in 1:nrow(W23)) {
      stop <- FALSE
      for (j in 1:i) {
        for (k in 1:i) {
          if (all(pmax(W23[j,],W23[k,])==W23[i,]) && rho[j] < rho[i] && rho[k] < rho[i]) {
            stop <- TRUE
            break
          }
        }
        if (stop) break
      }
      if (stop) next
      jir[l] <- i
      l <- l + 1
    }
    W24 <- W23[jir[1:(l-1)],]
    
    # Step 1.2: Sort the join-irreducible elements by cardinality (skip because it's already sorted)
    
    # Step 1.3: Compute the graph
    m0 <- MACC3
    
    for (i in nrow(W24):1) {
      xx <- W24[i,]
      if (all(xx==0)) next 
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z <- pmax(y - xx,0)
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(x) return(all(x == z))))
        if (!all(z==y)) {
          m0[w] <- m0[j] + m0[w]
        }
      }
    }
    
    return(m0)
  }
}
