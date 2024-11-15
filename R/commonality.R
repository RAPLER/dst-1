#' Compute qq from tt
#' 
#' qq is the commonality function as a set function from the subsets of the frame to \eqn{[0,1]}. To evaluate it, input a set encoded in binary vector, so the commonality number at that set can be returned.
#' 
#' @param tt Bolean description matrix
#' @param m Mass assignment vector of probabilities
#' @param method = NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt") or Efficient Zeta Transform on a join-closed subset ("ezt-j")
#' @param W2c = NULL: A binary matrix of support. Defaults to the complement of tt matrix
#' @return f Commonality function
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE),
#' m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' qq <- commonality(x$tt,x$spec[,2])
#' qq(c(1,0,0))
commonality <- function(tt, m, method = NULL, W2c = NULL){
  if (is.null(method)) {
    f <- function(x) {
      q <- 0
      for (i in 1:nrow(tt)) {
        if (all(tt[i,] - x >= 0)) {
          q <- q + m[i]
        }
      }
      return(q)
    }
  } else if (method=="fzt") {
    # Fast Zeta Transform
    m_seq <- rep(0, 2**ncol(tt))
    for (i in 1:nrow(tt)) {
      w <- decode(rep(2, ncol(tt)), tt[i,])
      m_seq[w + 1] <- m[i]
    }

    for (i in 1:ncol(tt)) {
      x <- rep(1,ncol(tt))
      x[i] <- 0
      for (j in 1:2**ncol(tt)) {
        y <- encode(rep(2, ncol(tt)), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, ncol(tt)), z)
        if (!all(z==y)) {
          m_seq[w + 1] <- m_seq[j] + m_seq[w + 1]
        }
      }
    }
    
    f <- function(x) {
      # print(m_seq)
      w <- decode(rep(2, ncol(tt)), x)
      q <- m_seq[w + 1]
      # print(q)
      return(q)
    }
    return(f)
  } else if (method=="ezt") {
    #
    # Use Efficient Zeta Transform
    #
    W2 <- tt
    MACC <- m
    names(MACC) <- rownames(W2)
    # Step 0.1 Insert complements of W2 into W2
    if (is.null(W2c)) {
      W2c <- 1-W2
      rownames(W2c) <- nameRows(1-W2)
    } else {
      colnames(W2c) <- colnames(W2)
      rownames(W2c) <- nameRows(W2c)
    }
    
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
    
    # TODO: compute values
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
    Q0 <- MACC3
    
    for (i in 1:nrow(W24)) {
      xx <- W24[i,]
      if (all(xx==0)) next 
      for (j in 1:nrow(W23)) {
        # print(i)
        # print(j)
        y <- W23[j,]
        z <- pmax(y,xx)
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(x) return(all(x == z))))
        # print(xx)
        # print(y)
        # print(z)
        if (!all(z==y) && length(w) != 0) {
          # print("update")
          Q0[j] <- Q0[j] + Q0[w]
        }
      }
    }
    
    f <- function(x) {
      # print(Q0)
      z <- t(as.matrix(x))
      colnames(z) <- colnames(W2)
      nz <- nameRows(z)
      w <- Q0[nz]
      # print(w)
      if(is.na(w)) {
        return(0)
      } else {
        return(unname(w))
      }
    }
    return(f)
  } else if (method=="ezt-j") {
    #
    # Efficient Zeta Transform on a join-closed subset
    #
    W2 <- tt
    MACC <- m
    names(MACC) <- rownames(W2)
    # Step 0.1 Insert complements of W2 into W2
    if (is.null(W2c)) {
      W2c <- 1-W2
      rownames(W2c) <- nameRows(1-W2)
    } else {
      colnames(W2c) <- colnames(W2)
      rownames(W2c) <- nameRows(W2c)
    }
    
    W21 <- rbind(W2,W2c)
    
    # Step 2.0.2 Insert closure elements
    W2x <- W21
    for (i in 1:nrow(W21)) {
      for (j in i:nrow(W21)) {
        # Step 2.0.2.1 insert meet-closure
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
    
    # TODO: compute values
    MACCc <- rep(0,nrow(W21)-nrow(W2))
    names(MACCc) <- rownames(W21)[(nrow(W2)+1):nrow(W21)]
    MACC1 <- c(MACC,MACCc)
    
    # Step 2.0.3 Remove duplicates
    MACC2 <- MACC1[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 2.0.4 Sort W2, MACC
    sort_order <- order(apply(W22,1,sum))
    W23 <- W22[sort_order,]
    MACC3 <- MACC2[sort_order]
    
    # Step 2.1: Find iota elements
    # Step 2.1.1: Find upsets of each singleton in W23
    # Step 2.1.2: Filter those that are non-empty
    # Step 2.1.3: Find infimum of each upset
    iota <- NULL
    for (i in 1:ncol(W23)) {
      ZZ <- rep(0,ncol(W23))
      ZZ[i] <- 1
      uZZ <- arrow(ZZ,W23,"up")
      if(is.null(nrow(uZZ)) || nrow(uZZ) > 0) { 
        inf_uZZ <- bound(if (is.null(nrow(uZZ))) t(as.matrix(uZZ)) else uZZ,"inf") 
      } else { 
        inf_uZZ <- NULL
      }
      iota <- rbind(iota, inf_uZZ)
    }
    W24 <- iota[!duplicated(iota),]
    
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
    
    f <- function(x) {
      z <- t(as.matrix(x))
      colnames(z) <- colnames(W2)
      nz <- nameRows(z)
      w <- Q0[nz]
      if(is.na(w)) {
        return(0)
      } else {
        return(unname(w))
      }
    }
    return(f)
  } else {
    stop("Input method must be one of NULL, fzt, ezt, ezt-j, ezt-m")
  }
}