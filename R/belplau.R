#' Calculation of the degrees of Belief and Plausibility of a basic chance assignment (bca).
#'
#'Degrees of Belief \code{Bel} and Plausibility \code{Pl} of the focal elements of a bca are computed. The ratio of the plausibility of a focal element against the plausibility of its contrary is also computed. Subsets with zero mass can be excluded from the calculations.\cr
#' @details The degree of belief \code{Bel} is defined by: \cr
#' \deqn{bel(A) = Sum((m(B); B \subseteq A))}{bel(A) = Sum((m(B); B <= A))} for every subset B of A.\cr
#' The degree of plausibility \code{pl} is defined by: \cr
#' \deqn{pl(A) = Sum[(m(B); B \cap A \neq \emptyset]}{pl(A) = Sum[(m(B); (B & A) not empty]} for every subset \code{B} of the frame of discernment. \cr
#' The plausibility ratio of a focal element \code{A} versus its contrary \code{not A} is defined by:  \eqn{Pl(A)/(1-Bel(A))}.
#' @param x A basic chance assignment mass function (see \code{\link{bca}}).
#' @param remove = TRUE: Exclude subsets with zero mass.
#' @param h = NULL: Hypothesis to be tested. Description matrix in the same format than \code{x$tt}
#' @param method = NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt") or Efficient Zeta Transform on a meet-closed subset ("ezt-m")
#' @return A matrix of \code{M} rows by 3 columns is returned, where \code{M} is the number of focal elements: \itemize{
#'  \item Column 1: the degree of Belief \code{bel};
#'  \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
#'  \item Column 3: the degree of Epistemic uncertainty \code{unc};
#'  \item Column 4: the degree of Plausibility \code{plau};
#'  \item Column 5: the Plausibility ratio \code{rplau}.
#'    }
#' @author Claude Boivin, Peiyuan Zhu
#' @export
#' @references \itemize{
#' \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 39-43.
#' \item Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
#' }
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' belplau(x)
#' y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
#' xy <- nzdsr(dsrwon(x,y))
#' belplau(xy)
#' print("compare all elementary events")
#' xy1 <- addTobca(x = xy, tt = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = TRUE))
#' belplau(xy1) 
#' belplau(xy1, remove = TRUE) 
#' belplau(xy1, h = matrix(c(1,0,0,0,1,1), nrow = 2, byrow = TRUE))
#' 
belplau<-function (x, remove = FALSE, h = NULL, method = NULL) {
  #
  # Local variables:  xtest, row_m_empty, MACC, W2, INUL, MACC, W2)
  # Functions calls: belplauH, nameRows, ttmatrix
  #
  # 1. Checking input data 
  #
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  # computation using description matrix tt
  #
  # use ssnames to reconstruct tt if null
  #
  if (is.null(x$spec)) {
    stop("Missing spec")
  } 
  if (is.null(x$tt) ) {
    if (is.null(x$ssnames) == FALSE) {
      z <- x$ssnames
      x$tt <- ttmatrix(z)
    }
    else {
      stop("No description matrix and no subsets names found. ")
    }
  }
  #
  # check if only one row, then convert to matrix
  xtest <- x$tt
  if (shape(shape(xtest)) < 2 ) {
    xtest <- matrix(xtest, nrow = 1)
  }
  #
  # check if m_empty present and if not 0
  if (sum((apply(xtest, 1, sum)) == 0) > 0) {
    row_m_empty <- match(1:nrow(xtest), rownames(xtest) == "\u00f8")
    row_m_empty <- row_m_empty[1]
    if (!is.na(row_m_empty)) {
      if (x$spec[row_m_empty,2] > 0.000001) {
    stop("Invalid data: Empty set among the focal elements. Normalization necessary. Apply function nzdsr to your bca to normalize your result.")
      }
    }
  }
  #
  # End checks and preparation
  #
  # 2. Prepare data for calculations of bel and pl functions
  #
  # vector of masses and description matrix tt needed for these calculations
  #
  MACC<-x$spec[,2]  # vector of masses
  W2 <- rbind(x$tt) # description matrix
  #
  # Case where user do not want subsets with zero mass (remove = TRUE)
  # Remove subsets with zero mass, but the frame
  #
  INUL<-c(MACC[-length(MACC)]>0,TRUE)
  if (remove != FALSE) {
    MACC<-MACC[INUL]
    W2 <- rbind(W2[INUL,])
  } 
  #
  # Use Fast Zeta Transform
  #
  if(is.null(method)) {
    stop("Input method cannot be NULL")
  } else if(method == "fzt") {
    
    bel <- rep(0, 2**ncol(W2))
    for (i in 1:nrow(W2)) {
      w <- decode(rep(2, ncol(W2)), W2[i,])
      bel[w + 1] <- MACC[i]
    }

    for (i in 1:ncol(W2)) {
      xx <- rep(0,ncol(W2))
      xx[i] <- 1
      for (j in 1:2**ncol(W2)) {
        y <- encode(rep(2, ncol(W2)), j - 1)
        z <- pmax(xx,y)
        w <- decode(rep(2, ncol(W2)), z)
        if (!all(z==y)) {
          bel[w + 1] <- bel[j] + bel[w + 1]
        }
      }
    }

    disbel <- rep(0, 2**ncol(W2))
    for (j in 1:2**ncol(W2)) {
      y <- encode(rep(2, ncol(W2)), j - 1)
      z <- decode(rep(2, ncol(W2)), 1 - y)
      disbel[z + 1] <- bel[j]
    }
    
    plau <- 1 - disbel
    rplau <- plau / (1 - bel)
    unc <- plau - bel
    resul <- cbind(bel,disbel,unc,plau,rplau)
    
    ltt <- lapply(X=0:(2**ncol(W2)-1), FUN = function(X) {encode(rep(2,ncol(W2)), X)})
    tt_abc <- matrix(unlist(ltt), ncol=ncol(W2), byrow = TRUE)
    colnames(tt_abc) <- unlist(x$valuenames)
    
    rownames(resul) <- nameRows(tt_abc)
    return(resul)
    
  } else if (method=="ezt") {
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
        # Step 0.1.1.1 insert join-closure
        z <- pmax(W21[i,],W21[j,])
        x <- which(apply(W2x, 1, function(x) return(all(x == z))))
        if (length(x) == 0) {
          z <- t(as.matrix(z))
          rownames(z) <- nameRows(z)
          W2x <- rbind(W2x,z)
        }
        
        # Step 0.1.1.2 insert meet-closure
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
    sort_order <-order(apply(W22,1,sum))
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
    bel0 <- MACC3
    
    for (i in nrow(W24):1) {
      xx <- W24[i,]
      if (all(xx==0)) next 
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z <- pmax(xx,y)
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(x) return(all(x == z))))
        if (!all(z==y)) {
          bel0[w] <- bel0[j] + bel0[w]
        }
      }
    }

    # Step 1.4: Compute the belplau table and output
    bel <- rep(0, length(MACC))
    disbel <- rep(0, length(MACC))
    for (j in 1:length(MACC)) {
      x <- which(apply(W23, 1, function(x) return(all(x == W2[j,]))))
      y <- which(apply(W23, 1, function(x) return(all(x == 1 - W2[j,]))))
      bel[j] <- bel0[x]
      if (length(y) > 0) disbel[j] <- bel0[y]
    }
    
    plau <- 1 - disbel
    rplau <- plau / (1 - bel)
    unc <- plau - bel
    resul <- cbind(bel,disbel,unc,plau,rplau)
    
    rownames(resul) <- nameRows(W2)
    
    return(resul)
  } else if (method=="ezt-m") {
    #
    # Efficient Zeta Transform on a meet-closed subset
    #
    # Step 2.0.1 Insert complements of W2 into W2
    W2c <- 1-W2

    rownames(W2c) <- nameRows(1-W2)
    W21 <- rbind(W2,W2c)
    
    # Step 2.0.2 Insert closure elements
    W2x <- W21
    for (i in 1:nrow(W21)) {
      for (j in i:nrow(W21)) {
        # Step 2.0.2.1 insert join-closure
        z <- pmax(W21[i,],W21[j,])
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
      MACCc <- rep(0,nrow(W21)-nrow(W2))
      names(MACCc) <- rownames(W21)[(nrow(W2)+1):nrow(W21)]
      MACC1 <- c(MACC,MACCc)
    } else {
      MACC1 <- MACC
    }
    
    # Step 2.0.3 Remove duplicates
    MACC2 <- MACC1[!duplicated(W21)]
    W22 <- W21[!duplicated(W21),]
    
    # Step 2.0.4 Sort W2, MACC
    sort_order <- order(apply(W22,1,sum))
    W23 <- W22[sort_order,]
    MACC3 <- MACC2[sort_order]

    # Step 2.1: Find dual iota elements
    # Step 2.1.1: Find downsets of each complement of singletons in W23
    # Step 2.1.2: Filter those that are non-empty
    # Step 2.1.3: Find supremum of each upset
    iota <- NULL
    for (i in 1:ncol(W23)) {
      ZZ <- rep(1,ncol(W23))
      ZZ[i] <- 0
      uZZ <- arrow(ZZ,W23,"down")
      if(is.null(nrow(uZZ)) || nrow(uZZ) > 0) { 
        sup_uZZ <- bound(if (is.null(nrow(uZZ))) t(as.matrix(uZZ)) else uZZ,"sup") 
      } else { 
        sup_uZZ <- NULL
      }
      iota <- rbind(iota, sup_uZZ)
    }
    W24 <- iota[!duplicated(iota),]
    
    # Step 2.2: Compute the graph
    
    # Step 2.2.1: Check if the first condition is satisfied
    # Step 2.2.1: Check if the second condition is satisfied
    bel0 <- MACC3
    
    for (i in 1:nrow(W24)) {
      xx <- W24[i,]
      for (j in 1:nrow(W23)) {
        y <- W23[j,]
        z0 <- arrow(pmin(xx,y), W23, "down")
        z <- bound(as.matrix(z0), "sup")
        # Find w, the position of z on the list W2
        w <- which(apply(W23, 1, function(s) return(all(s == z)))) 
        k0 <- W24[1:i,]
        if ((length(w) > 0) && (!all(z==y)) && 
            all((z - pmin(y,bound(if (is.null(nrow(k0))) t(as.matrix(k0)) else k0, "inf"))) >= 0)) {
          bel0[j] <- bel0[j] + bel0[w]
        }
      }
    }
    
    # Step 2.3: Compute belplau table
    bel <- rep(0, length(MACC))
    disbel <- rep(0, length(MACC))
    for (j in 1:length(MACC)) {
      x <- which(apply(W23, 1, function(x) return(all(x == W2[j,]))))
      y <- which(apply(W23, 1, function(x) return(all(x == 1 - W2[j,]))))
      bel[j] <- bel0[x]
      if (length(y) > 0) disbel[j] <- bel0[y]
    }
    
    plau <- 1 - disbel
    rplau <- plau / (1 - bel)
    unc <- plau - bel
    resul <- cbind(bel,disbel,unc,plau,rplau)
    
    rownames(resul) <- nameRows(W2)
    
    return(resul)
  } else {
    stop("Input method must be one of fzt, ezt, ezt-m")
  }
  
  #
  # 3.1 Check if there's hypothesis to be tested
  if (!is.null(h)) {
    # check that h is like x$tt
    if ((is.matrix(h) == FALSE) ) {
      stop("h parameter must be a (0,1) or logical matrix.")
    }
    if (ncol(h) != ncol(x$tt)) {
      stop("Error in input arguments: number of columns of h not equal to ncol(x$tt)") 
    }
    if (is.null(colnames(h)) ) {
      colnames(h) <- colnames(x$tt)
      rownames(h) <- nameRows(h)
    }  
    resul <- belplauH(MACC,W2,h) 
  } else {
    resul <- belplauH(MACC, W2, h = W2) 
  }
  return(resul)
}

