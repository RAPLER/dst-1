#' Compute qq from tt
#' 
#' qq is the commonality function as a set function from the subsets of the frame to \eqn{[0,1]}. To evaluate it, input a set encoded in binary vector, so the commonality number at that set can be returned.
#' 
#' @param tt Bolean description matrix
#' @param m Mass assignment vector of probabilities
#' @param method = NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt") or Efficient Zeta Transform on a meet-closed subset ("ezt-m")
#' @return f Commonality function
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2],  method = "ezt")
#' qq
#' qq1 <- commonality(x$tt,x$spec[,2])
#' qq1
commonality <- function(tt, m, method = NULL){
  # Checks
  if (isS4(tt) == TRUE ) {
    tt <- as.matrix(tt) # convert sparse to matrix. to be able to apply duplicated() operator and other matrix operators.
  }
  if (is.null(method)) {
    Q0 <- rep(0, 2**ncol(tt))
    for (i in 1:length(Q0)) {
      y <- encode(rep(2, ncol(tt)), i - 1)
      yy <- t(as.matrix(y))
      colnames(yy) <- colnames(tt)
      names(Q0)[i] <- nameRows(yy)
    }
    for (i in 1:nrow(tt)) {
      for (j in 1:length(Q0)) {
        w <- encode(rep(2,ncol(tt)),j - 1)
        if (all(tt[i,] - w >= 0)) {
          Q0[j] <- Q0[j] + m[i]
        }
      }
    }
    return(Q0)
  } else if (method=="fzt") {
    # Fast Zeta Transform
    Q0 <- rep(0, 2**ncol(tt))
    
    for (i in 1:length(Q0)) {
      y <- encode(rep(2, ncol(tt)), i - 1)
      yy <- t(as.matrix(y))
      colnames(yy) <- colnames(tt)
      names(Q0)[i] <- nameRows(yy)
    }
    
    for (i in 1:nrow(tt)) {
      w <- decode(rep(2, ncol(tt)), tt[i,])
      Q0[w + 1] <- m[i]
    }

    for (i in 1:ncol(tt)) {
      x <- rep(1,ncol(tt))
      x[i] <- 0
      for (j in 1:2**ncol(tt)) {
        y <- encode(rep(2, ncol(tt)), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, ncol(tt)), z)
        if (!all(z==y)) {
          Q0[w + 1] <- Q0[j] + Q0[w + 1]
        }
      }
    }
    
    return(Q0)
  } else if (method=="ezt") {
    #
    # Efficient Zeta Transform: fig 5, cor 3.2.4 
    #
    W21 <- tt
    MACC <- m
    names(MACC) <- rownames(W21)
    
    # Step 0.2 Remove duplicates
    MACC2 <- MACC[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 1.0: Sort W2, MACC
    sort_order <- order(apply(W22,1,function(x) decode(rep(2,ncol(W22)),x)))
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
    Q0 <- MACC3
    
    for (i in 1:nrow(W24)) {
      xx <- W24[i,]
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z <- pmax(y,xx)
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(x) return(all(x == z))))
        if (!all(z==y) && length(w) != 0) {
          Q0[j] <- Q0[j] + Q0[w]
        }
      }
    }
    
    return(Q0)
  } else if (method=="ezt-m") {
    #
    # Efficient Zeta Transform on a meet-closed subset: fig 7, thm 3.2.2. 
    #
    W21 <- tt
    MACC <- m
    names(MACC) <- rownames(W21)
    
    # Step 2.0.3 Remove duplicates
    MACC2 <- MACC[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 2.0.4 Sort W2, MACC
    sort_order <- order(apply(W22,1,sum))
    W23 <- W22[sort_order,]
    MACC3 <- MACC2[sort_order]
    
    # Step 2.1: Find iota elements
    # Step 2.1.1: Find upsets of each singleton in W23
    # Step 2.1.2: Filter those that are non-empty
    # Step 2.1.3: Find infimum of each upset
    iota <- list()
    for (i in 1:ncol(W23)) {
      ZZ <- rep(0,ncol(W23))
      ZZ[i] <- 1
      uZZ <- arrow(ZZ,W23,"up")
      if(is.null(nrow(uZZ)) || nrow(uZZ) > 0) { 
        inf_uZZ <- bound(if (is.null(nrow(uZZ))) t(as.matrix(uZZ)) else uZZ,"inf") 
      } else { 
        inf_uZZ <- NULL
      }
      iota <- append(iota, list(inf_uZZ))
    }
    iota <- do.call(rbind, lapply(iota, as.logical))
    W24 <- iota[!duplicated(iota),]
    
    # Step 2.1.4 Sort W24
    sort_order <- order(apply(W24,1,sum))
    W24 <- W24[sort_order,]

    # Step 2.2: Compute the graph
    
    # Step 2.2.1: Check if the first condition is satisfied
    # Step 2.2.1: Check if the second condition is satisfied
    Q0 <- MACC3
    
    for (i in 1:nrow(W24)) {
      xx <- W24[i,]
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z0 <- arrow(pmax(xx,y), W23, "up")
        z <- bound(as.matrix(z0), "inf")
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(s) return(all(s == z)))) 
        k0 <- W24[1:i,]
        if ((length(w) > 0) && (!all(z==y)) && 
            all((pmax(y,bound(if (is.null(nrow(k0))) t(as.matrix(k0)) else k0, "sup")) - z) >= 0)) {
          Q0[j] <- Q0[j] + Q0[w]
        }
      }
    }
    
    return(Q0)
  } else {
    stop("Input method must be one of NULL, fzt, ezt, ezt-m")
  }
}