#' Combination of two mass functions
#' 
#'The unnormalized Dempster's rule is used to combine two mass functions \code{mx} and \code{my} defined  on the same frame of discernment and represented by their respective basic chance assignments \code{x}  and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice  to the user to normalize the results or not (for the normalization operation, see \code{\link{nzdsr}}).
#' @details The two bca's \code{x} and \code{y} must be defined on the same frame of discernment for the combination to take place. The relation number of the x input is given to the output result.  
#' @param x A  bca mass function (see bca). (see \code{\link{bca}}).
#' @param y A  bca mass function (see bca).
#' @return A list of class bcaspec with these two components added: \itemize{
#'   \item I12 Intersection table of subsets.
#'   \item Sort_order Sort order of subsets.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' x1 <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"),  
#' infovarnames = "x", varnb=1)
#' x2 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, 
#' byrow = TRUE), m=c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  
#' infovarnames = "x", varnb = 1)
#' dsrwon(x1,x2)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
dsrwon<-function(x,y) {
  if ( (inherits(x, "bcaspec") == FALSE) | (inherits(y, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  x1<-rbind(x$tt)  # (M x K) matrix
  y1<-rbind(y$tt)  # (N x K) matrix
  nc1<-ncol(x1)
  nc2<-ncol(y1)
  if (nc1 != nc2) {
    stop("Nb of elements of frame x and frame y not equal.") 
  }
  n <- x$infovar[,1]  # numbers of the variables 
  # extract the masses. This new version works without $combination
  vx <- x$spec[,2]
  vy <- y$spec[,2]
    if (is.null(vx) | is.null(vy)) {
      vx <- x$spec[,2]
      vy <- y$spec[,2]
    }
  V12<-outer(vx,vy, "*")     # compute masses
  ## transform table of intersections: (M x N) rows by K 
  N12<-inters(x1,y1)         # intersection of the subsets
  N12<-aperm(N12,c(2,1,3))   # transformation
  N12<-array(c(N12),c(dim(N12)[1],prod(dim(N12)[-1])), dimnames= list(colnames(x1), as.vector(outer(rownames(x1), rownames(y1) , FUN="paste"))))
  
  N12<-aperm(N12,c(2,1)) 
  ## remove duplicates from the table
  W1<- doubles(N12)      
  rownames(W1) <- nameRows(W1)
  I12<-dotprod(W1,aperm(N12,c(2,1)),g="&",f="==")    ## idendify  contributions to each subset
  MAC<-apply(I12*t(array(V12,dim(t(I12)))),1,sum)     ## calculate total mass of each subset 
  ## order the subsets to check if there is the empty subset and where it is.
  sort_order<-order(apply(W1,1,sum))
  z<- sum(W1[sort_order[1],])
  ## identify the place of the empty set 
  if (z==0) empty<-sort_order[1] else empty<-0 
  tt<-matrix(W1,ncol=nc1, dimnames = dimnames(W1))
  ## calculate the mass of the empty set
  if (z==0) m_empty<-MAC[empty] else m_empty<-0
  ## Measure of conflict. Code to be revised
  con12<-1-(1-x$con)*(1-y$con)
  if ((con12 == 1) | (m_empty == 1)) { 
    warning('Totally conflicting evidence (con = 1). Data is inconsistent.')}
  con<-1-(1-con12)*(1-m_empty)
  ## result
  mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
  spec <- cbind((1:nrow(tt)), mMAC)
  colnames(spec) <- c("specnb", "mass")
  infovar <- x$infovar
  infovaluenames <- x$infovaluenames
  # inforel parameter
  inforel <- x$inforel
  ## fin test
  # construction of the result
  z <- list(con = con, tt=tt, spec = spec, infovar = infovar, infovaluenames = infovaluenames, inforel = inforel, I12=I12, sort_order=sort_order)
  class(z) <- append(class(z), "bcaspec")
  return(z)
  }
 