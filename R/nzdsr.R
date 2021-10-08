#' Normalization of a basic chance assignment
#'
#' It may occur that the result of the combination of two basic chance assignments with Dempster's Rule of combination contains a non-zero mass allocated to the empty set. The function \code{nzdsr} normalizes the result of function \code{dsrwon} by dividing the mass value of the non-empty subsets by 1 minus the mass of the empty set. 
#' @param x A basic chance assignment, i.e. a object of class bcaspec.
#' @return z The normalized basic chance assignment.
#' @author Claude Boivin, Stat.ASSQ
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
#' @examples 
#' x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.9,0.1), cnames = c("yes", "no"),
#' varnames = "x", varnb = 1)
#' x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.5,0.5), cnames = c("yes", "no"), 
#' varnames = "x", varnb = 1)
#' print("combination of x1 and x2")
#' x1x2 <- dsrwon(x1,x2, varname = "x")
#' nzdsr(x1x2) 
#' 
#' print("normalization of a bca definition.")
#' y2 <- bca(f = matrix(c(0,0,0,1,0,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5,0.3), 
#' cnames = c("a", "b", "c"), varnb = 1)
#' nzdsr(y2)  
#' @export
#' 
nzdsr<-function(x) {
  #
  # Local variables: nc, vacuous, w1, w12, mac, MACC, empty, m_empty, tri, ind
  #
  ## 1. Checks
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  if (x$con == 1) { 
    stop('Completely conflicting evidence (con = 1). Data is inconsistent.')}
  #
  ## 2. Reconstruct I12 matrix (need to be updated if missing or if function addTobca has been used to add subsets)
  nc <- ncol(x$tt)
  vacuous <- bca(matrix(rep(1, nc), nrow=1), m=1)
  vacuous$valuenames <- x$valuenames
  x <- dsrwon(x,vacuous)
  #
  # 3. Assign variables
  #
  w12<-cbind(x$spec[,2], x$tt)
  w1<- x$tt
  mac<-x$spec[,2]
  nc=ncol(w1) 
  tri<-x$sort_order
  #
  # 4. remove empty set and normalize masses
  #
  ind <- w12[tri[1],]
  if ((ind[1] != 0) & (sum(ind[-1]) == 0)) {
    empty <- tri[1]  
    W2 <- matrix(w1[tri[-1],], ncol=nc)
    ## calculate normalized masses
    MACC <- mac[tri[-1]]/(1-mac[empty]) 
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
  #
  # tt matrix
  tt <- W2
  colnames(tt) <- colnames(x$tt)
  rownames(tt) <- nameRows(tt)
  #
  # spec parameter
  #
  spec <- cbind((1:nrow(tt)), MACC)
  colnames(spec) <- c("specnb", "mass")
  #
  # infovar, varnames, valuenames, inforel parameters
  #
  infovar <- x$infovar
  varnames <- x$varnames
  valuenames <- x$valuenames
  relnb <- (x$inforel)[1,1]
  inforel <- matrix(c(relnb, nrow(infovar)), ncol = 2)
  colnames(inforel) <- c("relnb", "depth") 
  #
  # construction of the result
  #
  z <- list(con=x$con, tt = tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
  class(z) <- append(class(z), "bcaspec")
  return(z)
    }