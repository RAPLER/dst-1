#' Normalization of results from Dempster's rule of combination
#'
#' The combination of two bca distributions may produce a distribution with a non zero mass allocated to the empty set. This function produces a normalized distribution by dividing the focal elements (other than the empty set) by 1 minus the mass of the empty set.
#' @param x A list of class bcaspec, normally the result of the combination of two belief functions that we want to normalize. (see \code{\link{dsrwon}}). A belief function in its bca form (see \code{\link{bca}}) may also be submitted.
#' @param infovarname A name can be given to the resulting variable. Named "nv1" if missing.
#' @return A list in the bca form, namely: \itemize{
#'   \item $con The measure of conflict.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 57-61: Dempster's rule of combination.
#' @examples 
#' x1 <- bca(f=matrix(c(1,0,1,1),nrow=2, byrow = TRUE), m=c(0.9,0.1), cnames =c("yes", "no"),infovarnames = "x1", varnb = 1)
#' y1 <- bca(f=matrix(c(0,1,1,1),nrow=2, byrow = TRUE), m=c(0.5,0.5), cnames =c("yes", "no"), varnb = 1)
#' print("combination of x1 and y1")
#' x1y1 <- dsrwon(x1,y1)
#' nzdsr(x1y1) 
#' 
#' print("normalization of a bca definition.")
#' y2 <- bca(f=matrix(c(0,0,0,1,0,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5,0.3), cnames =c("a", "b", "c"), varnb = 1)
#' nzdsr(y2)  
#' @export
#' 
nzdsr<-function(x, infovarnames = NULL) {
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
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
  con<-x$con
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