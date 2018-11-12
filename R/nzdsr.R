#' Normalization of a bca mass function
#'
#' It may occur that the result of the combination of two mass functions with Dempater'Rule of combination contains a non-zero mass allocated to the empty set. The function \code{nzdsr} normalizes the result of function \code{dsrwon} by dividing the mass value of the non-empty subsets by 1 minus the mass of the empty set. 
#' @param x A mass function, i.e. a list of class bcaspec..
#' @return The normalized bca mass function inputted.
#' @author Claude Boivin, Stat.ASSQ
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
#' @examples 
#' x1 <- bca(f=matrix(c(1,0,1,1),nrow=2, byrow = TRUE), 
#' m=c(0.9,0.1), cnames =c("yes", "no"),
#' infovarnames = "x", varnb = 1)
#' x2 <- bca(f=matrix(c(0,1,1,1),nrow=2, byrow = TRUE), 
#' m=c(0.5,0.5), cnames =c("yes", "no"), 
#' infovarnames = "x", varnb = 1)
#' print("combination of x1 and x2")
#' x1x2 <- dsrwon(x1,x2)
#' nzdsr(x1x2) 
#' 
#' print("normalization of a bca definition.")
#' y2 <- bca(f=matrix(c(0,0,0,1,0,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5,0.3), 
#' cnames =c("a", "b", "c"), varnb = 1)
#' nzdsr(y2)  
#' @export
#' 
nzdsr<-function(x) {
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  con<-x$con
  if (con == 1) { 
    stop('Completely conflicting evidence (con = 1). Data is inconsistent.')}
  if (is.null(x$I12)) {
    nc <- ncol(x$tt)
    frame <- bca(matrix(rep(1, nc), nrow=1), m=1)
    x <- dsrwon(x,frame)
  }
  w12<-cbind(x$spec[,2], x$tt)
  w1<- x$tt
  mac<-x$spec[,2]
  i12<-x$I12
  nc=ncol(w1) 
  tri<-x$sort_order
  ## remove empty set
  ind<-w12[tri[1],]
  if ((ind[1]!=0) & (sum(ind[-1])==0)) {
    empty<-tri[1]  
    W2<-matrix(w1[tri[-1],],ncol=nc)
    ## calculate normalized masses
    MACC<-mac[tri[-1]]/(1-mac[empty]) 
    m_empty<-mac[empty] 
  } 
  else {
    empty<-0 
    W2<-matrix(w1,ncol=nc)
    MACC<-mac
    m_empty<-0
  }
  tt <- W2
  colnames(tt) <- colnames(x$tt)
  rownames(tt) <- nameRows(tt)
  spec <- cbind((1:nrow(tt)), MACC)
  colnames(spec) <- c("specnb", "mass")
  # infovar parameter
  infovar <- x$infovar
  # infovaluenames parameter
  infovaluenames <- x$infovaluenames
  # inforel parameter
  relnb <- (x$inforel)[1,1]
  inforel <- matrix(c(relnb, nrow(infovar)), ncol = 2)
  colnames(inforel) <- c("relnb", "depth") 
  # construction of the result
  z <- list(con=con, tt = tt, spec = spec, infovar = infovar, infovaluenames = infovaluenames, inforel = inforel)
  class(z) <- append(class(z), "bcaspec")
  return(z)
    }