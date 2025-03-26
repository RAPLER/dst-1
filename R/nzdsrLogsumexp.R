#' Normalization of a basic chance assignment with logsumexp
#'
#' It may occur that the result of the combination of two basic chance assignments with Dempster's Rule of combination contains a non-zero mass allocated to the empty set. The function \code{nzdsr} normalizes the result of function \code{dsrwon} by dividing the mass value of the non-empty subsets by 1 minus the mass of the empty set. 
#' @param x A basic chance assignment, i.e. a object of class bcaspec.
#' @param sparse Put "yes" to use sparse matrix. Default = "no".
#' @return z The normalized basic chance assignment.
#' @author Claude Boivin, Peiyuan Zhu
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
#' @examples 
#' x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.9,0.1), cnames = c("yes", "no"),
#' varnames = "x", idvar = 1)
#' x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.5,0.5), cnames = c("yes", "no"), 
#' varnames = "x", idvar = 1)
#' print("combination of x1 and x2")
#' x1x2 <- dsrwon(x1,x2, varname = "x")
#' nzdsr(x1x2) 
#' # Test with sparse matrices
#' x1s=x1
#' x2s=x2
#' x1s$tt <- methods::as(x1$tt, "RsparseMatrix")
#' x2s$tt <- methods::as(x2$tt, "RsparseMatrix")
#' x1x2s <- dsrwon(x1s, x2s, use_ssnames = TRUE)
#' nzdsr(x1x2s)
#' 
#' print("normalization of a bca definition.")
#' y2 <- bca(tt = matrix(c(0,0,0,1,0,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5,0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
#' cat("y2")
#' cat("\  ")
#' y2
#' nzdsr(y2)  
#' @export
#' 
nzdsrLogsumexp<-function(x, sparse="no") {
  #
  # Local variables: nc, vacuous, w1, w12, mac, MACC, empty, m_empty, tri, ind, m0, Q
  # Functions calls: nameRows
  #
  ## 1. Checks
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  
  #
  # case of tt matrix missing
  if (is.null(x$tt)) {
    if(sparse=="yes") {
      x$tt <- ttmatrix(x$ssnames, "yes")
    } else {
      x$tt <- ttmatrix(x$ssnames)
    }
  }
  
  #
  # 3. Assign variables
  w12<-cbind(x$spec[,2], x$tt)
  w1<- x$tt
  mac<-x$spec[,2]
  nc=ncol(w1) 
  #
  ## 2023-06-26 test reconstruct sort_order if missing
  if (is.null(x$sort_order)) {
    x$sort_order<-order(apply(x$tt,1,sum))
  }
  # End 2023-06-26 Test
  #
  tri<-x$sort_order
  # 4. remove empty set and normalize masses
  ind <- w12[tri[1],]
  if ((ind[1] != 0) & (sum(ind[-1]) == 0)) {
    empty <- tri[1]  
    W2 <- matrix(w1[tri[-1],], ncol=nc)
    ## calculate normalized masses
    MACC <- exp(log(mac[tri[-1]])-log1p(-mac[empty])) 
    m_empty <- mac[empty] 
  } 
  else {
    empty<-0 
    W2 <- matrix(w1,ncol=nc)
    MACC <- mac
    m_empty <- 0
  }
  #
  # 5. Update bca parameters 
  # tt matrix
  tt <- W2
  colnames(tt) <- colnames(x$tt)
  rownames(tt) <- nameRows(tt)
  #
  # spec parameter
  spec <- cbind((1:nrow(tt)), MACC)
  colnames(spec) <- c("specnb", "mass")
  
  #
  # Computation of the conflict indice
  #
  con <- 1-(1-x$con)*(1-m_empty)
  
  #
  # infovar, varnames, valuenames, inforel parameters
  infovar <- x$infovar
  varnames <- x$varnames
  valuenames <- x$valuenames
  relnb <- (x$inforel)[1,1]
  inforel <- matrix(c(relnb, nrow(infovar)), ncol = 2)
  colnames(inforel) <- c("relnb", "depth") 
  #
  # construction of the result
  z <- list(con = con, tt = tt, qq = NULL, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
  class(z) <- append(class(z), "bcaspec")
  #
  return(z)
}