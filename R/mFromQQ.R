#' Construct a mass vector from qq function and ttmatrix of focal elements recursively.
#' 
#' @param qq Commonality function
#' @param n Frame dimension
#' @param cnames: A character vector containing the names of the elements of the frame of discernment
#' @param method = NULL: Use Fast Mobius Transform ("fmt") or Efficient Mobius Transform ("emt") or Efficient Mobius Transform on a meet-closed subset ("emt-m") 
#' @return m A corresponding mass vector
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' tt<- t(matrix(c(1,0,1,1),ncol = 2))
#' m<- c(.9,.1)
#' cnames <- c("yes","no")
#' x<- bca(tt, m, cnames=cnames)
#' mFromQQ(x$qq, 2, method = "fmt", cnames = cnames)
mFromQQ <- function(qq, n, cnames, method = NULL) {
  # Obtain tt matrix from commonality function
  #
  # Check that the input qq is a function
  if (is.numeric(qq) == FALSE) {
    stop("Input qq must be a numeric vector")
  }
  
  if (is.null(method)) {
    # Mobius inversion
    m0 <- rep(0, 2**n)
    for (i in 1:length(m0)) {
      w <- encode(rep(2, n), i - 1)
      names(w) <- cnames
      cname <- nameRows(t(as.matrix(w)))
      names(m0)[i] <- cname
      
      m <- 0
      j <- 0
      while (j <= (2**(length(w) - sum(w)) - 1)) {
        hh <- w
        dhh <- encode(rep(2, length(w) - sum(w)), j)
        hh[which(w == 0)] <- dhh
        
        hhn <- nameRows(t(as.matrix(hh)))
        
        m <- m + (-1) ** sum(dhh) * qq[hhn]
        j <- j + 1
      }
      m0[i] <- m
    }
    
    return(m0)
  } else if (method=="fmt") {
    # Fast Mobius Transform
    m0 <- rep(0, 2**n)
    for (i in 1:length(qq)) {
      w <- encode(rep(2, n), i - 1)
      names(w) <- cnames
      cname <- nameRows(t(as.matrix(w)))
      m0[i] <- qq[cname]
    }
    
    for (j in 1:2**n) {
      y <- encode(rep(2, n), j - 1)
      yy <- t(as.matrix(y))
      colnames(yy) <- cnames
      names(m0)[j] <- nameRows(yy)
    }
    
    for (i in 1:n) {
      x <- rep(1,n)
      x[i] <- 0
      for (j in 1:2**n) {
        y <- encode(rep(2, n), j - 1)
        z <- pmin(x,y)
        w <- decode(rep(2, n), z)
        if (!all(z==y)) {
          m0[w + 1] <- m0[w + 1] - m0[j]
        }
      }
    }
    
    return(m0)
  } else if (method == "emt") {
    # Load qq
    MACC <- qq
    
    # Load tt
    W2 <- matrix(rep(0,length(qq) * n), nrow = length(qq), ncol = n)
    colnames(W2) <- cnames
    for (i in 1:nrow(W2)) {
      if (names(qq[i]) == "\u00f8") { 
        next 
      } else if (names(qq[i]) == "frame") { 
        W2[i,] <- rep(1,n)
      } else { 
        W2[i,] <- (cnames %in% trimws(unlist(strsplit(names(qq[i]) , "\\+")))) 
      }
    }
    W21 <- W2
    
    #
    # Efficient Mobius Transform: fig 6, cor 3.2.5
    #
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
    
    if (nrow(W21)-nrow(W2) > 0) {
      MACCc <- rep(0,nrow(W21)-nrow(W2))
      for (i in 1:(nrow(W21)-nrow(W2))) {
        for (j in 1:nrow(W2)) {
          if (all((W2[j,] - W21[nrow(W2)+i,]) >= 0)) {
            MACCc[i] <- unname(MACC[j])
            break
          }
        }
      }
    } else {
      MACCc <- NULL
    }
    names(MACCc) <- rownames(W21)[(nrow(W2)+1):nrow(W21)]
    MACC1 <- c(MACC,MACCc)
    
    # Step 0.2 Remove duplicates
    MACC2 <- MACC1[!duplicated(W21)]
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
    m0 <- MACC3
    
    #print(m0)
    for (i in nrow(W24):1) {
      xx <- W24[i,]
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z <- pmax(y,xx)
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(x) return(all(x == z))))
        if (!all(z==y) && length(w) != 0) {
          m0[j] <- m0[j] - m0[w]
        }
      }
      #print(m0)
    }
    
    return(m0)
  } else if (method=="emt-m") {
    # Load qq
    MACC <- qq
    
    # Load tt
    W2 <- matrix(rep(0,length(qq) * n), nrow = length(qq), ncol = n)
    colnames(W2) <- cnames
    for (i in 1:nrow(W2)) {
      if (names(qq[i]) == "\u00f8") { 
        next 
      } else if (names(qq[i]) == "frame") { 
        W2[i,] <- rep(1,n)
      } else { 
        W2[i,] <- (cnames %in% trimws(unlist(strsplit(names(qq[i]) , "\\+")))) 
      }
    }
    W21 <- W2
    #
    # Efficient Mobius Transform on a meet-closed subset: fig 8, cor 3.2.6
    #
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
    
    if((nrow(W21)-nrow(W2))>0) {
      MACCc <- apply(W21[(nrow(W2)+1):nrow(W21),],1,qq)
      MACC1 <- c(MACC,MACCc)
    } else {
      MACC1 <- MACC
    }
    
    # Step 2.0.3 Remove duplicates
    MACC2 <- MACC1[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 2.0.4 Sort W2, MACC
    sort_order <- order(apply(W22,1,function(x) decode(rep(2,ncol(W22)),x)))
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
    
    # Sort iota elements
    sort_order <- order(apply(W24,1,sum))
    W24 <- W24[sort_order,]
    
    # Step 2.2: Compute the graph
    
    # Step 2.2.1: Check if the first condition is satisfied
    # Step 2.2.1: Check if the second condition is satisfied
    m0 <- MACC3
    
    for (i in nrow(W24):1) {
      xx <- W24[i,]
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z0 <- arrow(pmax(xx,y), W23, "up")
        z <- bound(if (is.null(nrow(z0))) t(as.matrix(z0)) else as.matrix(z0), "inf")
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(s) return(all(s == z)))) 
        k0 <- W24[1:i,]
        if ((length(w) > 0) && (!all(z==y)) && 
            all((pmax(y,bound(if (is.null(nrow(k0))) t(as.matrix(k0)) else k0, "sup")) - z) >= 0)) {
          m0[j] <- m0[j] - m0[w]
        }
      }
    }
    
    return(m0)
  } else {
    stop("Input method must be one of fmt, emt, emt-m")
  }
}
