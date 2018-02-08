#' Combination of two belief functions
#' 
#' The unnormalized Dempster's rule is used to combine two belief functions Bel1 and Bel2 defined on the same frame of discernment and represented by their respective basic chance assignments \code{x} and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice to the user to normalize the results or not (See \code{\link{nzdsr}}).
#' @details A function (inters) does the intersection of the propositions. Another function (doubles) remove duplicate elements from the result of the intersection of the M propositions of the first bca with the N propositions of the second bca. The two bcas \code{a} and \code{b} must be defined on the same frame of discernment for the intersection of propositions to be meaningful.   
#' @param x A belief function in its bca form (see \code{\link{bca}}).
#' @param y A belief function represented by its bca form.
#' @param infovarname A name can be given to the resulting variable. Named "v1" if missing.
#'  @param relnb A number can be given to the resulting relation. if omitted, the numbers of the input relations will be kept.
#' @return A list of five elements: \itemize{
#'   \item $combination: The matrix f of focal elements with a column of masses added.
#'   \item $I12 A table of intersections between subsets.
#'   \item $sort_order Indices for the sort of the propositions.
#'   \item $con: the measure of conflict between beliefs.
#'   \item $n: the node number.
#'  \item $inforel. A two column matrix containing variable numbers and the depth of the relation.
#'   }
#'   @details The relation number of the x input is given to the result.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' x1 <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"),  infovarnames = "x", n=1)
#' x2 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "x", n=1)
#' dsrwon(x1,x2)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 57-61: Dempster's rule of combination.
dsrwon<-function(x,y, infovarnames = NULL, relnb = NULL) {
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
  # extract the masses. Future version will work without $combination
  vx <- x$combination[,1]
  vy <- y$combination[,1]
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
  con<-1-(1-con12)*(1-m_empty)
  ## result
  mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
  spec <- cbind((1:nrow(tt)), mMAC)
  colnames(spec) <- c("specnb", "mass")
  # infovar parameter
  # test
#  infovar <- matrix(c(n, ncol(tt)), ncol = 2) erreur ici?
#   colnames(infovar) <- c("varnb", "size")
  infovar <- x$infovar
  # infovaluenames parameter
  # test
 # cnames <- colnames(W1)
  # if (missing(infovarnames)) {
  #   infovaluenames <- split(cnames, rep(paste(rep("v", nrow(infovar)),c(1:nrow(infovar)),sep=""), infovar[,2]))
  # } else {
  #   infovaluenames <- split(cnames,rep(infovarnames, infovar[,2]))
  # }
  infovaluenames <- x$infovaluenames
  # inforel parameter
  if (is.null(relnb)) {
    relnb <- (x$inforel)[,1]  # revoir
    relnb2 <- (y$inforel)[,1]
      if (relnb != relnb2) {
        relnb <- c(relnb, relnb2)
      }
  }
    inforel <- matrix(c(relnb, rep(nrow(infovar), length(relnb))), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
  # construction of the result
  W2<-cbind(mMAC,tt)
  z <- list(combination=W2,con = con, n = x$n, tt=tt, spec = spec, infovar = infovar, infovaluenames = infovaluenames, inforel = inforel, I12=I12, sort_order=sort_order)
  class(z) <- append(class(z), "bcaspec")
  return(z)
  }
 