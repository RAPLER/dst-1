#' Add some elements of 0 mass to an existing basic chance assignment. 
#'
#' Given a previously defined basic chance assignment (bca), the user may want to add some elements of the set of possible values or some subsets, even if they have zero mass value. This feature is useful, for example, to examine the measure of plausibility of these elements or subsets of zero mass value.
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param tt A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix \code{tt} must match the number of columns of the \code{tt} matrix of \code{x} (see \code{\link{bca}}). Each row of the matrix identify a subset of the set of possible values.
#' @param f Deprecated. Old name for \code{tt} matrix. 
#' @return x The original basic chance assignment \code{x} augmented with the added subsets defined by \code{tt}.
#' @author Claude Boivin
#' @export
#' @examples  
#' y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), 
#' m = c(0.6, 0.4),  cnames = c("a", "b", "c"), idvar = 1)
#' addTobca(y, matrix(c(0,1,0,0,0,1, 0,1,1), nrow = 3, byrow = TRUE))
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
#' xy <- dsrwon(x,y)
#' xy1 <- addTobca(nzdsr(xy), matrix(c(0,1,0,0,0,1), nrow = 2, byrow = TRUE))
#' xy1
#' # add all singletons to a bca
#' addTobca(x, tt = diag(rep(1, ncol(x$tt) ) )  ) 
#' 
addTobca <- function(x, tt, f) {
  #
  # Local variables: zt1, zt2, tt1 
  # Functions calls: dotprod, reduction 
  #
  # 0. Catch old parameters names, if any and replace by the new ones
  #
  # catch old parameter f and replace by tt if used instead of tt
  # calls <- names(sapply(match.call(), deparse))[-1]
  # if(any("f" %in% calls) & missing(tt)) {
  #   warning("Parameter name 'f' is deprecated. Use 'tt' instead.")
  #   tt <- f
  # }
  #
  # 1. Parameter checks
  #
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input x not of class bcaspec.")
  }
  if ((is.matrix(tt) ==FALSE) ) {
    stop("tt parameter must be a (0,1) or logical matrix.")
  }
  if (isS4(x$tt) == TRUE) {
    x$tt <- as.matrix(x$tt)
  }
    if (ncol(x$tt) != ncol(tt)) {
    stop("Error in input arguments: number of columns of tt not equal to ncol(x$tt)") 
    }
  #
  # 2. Calculations
  # 2.1 Check for replicates 
  #
  zt1 <- dotprod(tt, t(x$tt), g = "&", f = "==")
  zt2 <- apply(zt1, MARGIN = 1, FUN = "reduction", f = "|")
  tt1 <- tt[!zt2,]
  if (is.matrix(tt1) == FALSE) {
    tt1 <- matrix(tt1,ncol = length(tt1), dimnames = list(NULL, names(tt1)))
  }
  if (nrow(tt1) == 0) {
    # No new subset submitted 
    return(x)  
  } 
  #
  # 2.2 transform tt matrix of x and retain status of rows (old and new)
  x1 <- cbind(x$tt,0)
  tt1 <- cbind(tt1,1)
  ztt <- rbind(tt1,x1)
 # rownames(ztt) <- nameRows(ztt)
  ## 2.3 Order the subsets to find if the empty subset is there. Put empty set in first position of tt matrix
  sort_order<-order(apply(ztt,1,sum))
  ztt <- ztt[sort_order,]
  x$tt <- ztt[,-ncol(ztt)]
  if (is.matrix(x$tt) == FALSE) {
    x$tt <- matrix(x$tt,ncol = length(x$tt), dimnames = list(NULL, names(x$tt)))
  }
 rownames(x$tt) <- nameRows(x$tt)
  status <- ztt[,ncol(ztt)]
  ## 2.4 Identify if the empty set is present and define m_empty accordingly with it mass
  #
  mass <- c(rep(0, sum(!zt2)), x$spec[,2])
  z<- sum(x$tt[sort_order[1],])
  if (z==0) {
    empty<-sort_order[1]  
    m_empty<-mass[empty] 
  } else {
    empty<-0
    m_empty<-0
  }
  #
  # 2.5 Put masses in the same order as the tt matrix
  mass <- mass[sort_order]
  mMAC <-matrix(mass,ncol=1, dimnames =list(NULL, "mass"))
  #
  ## 2.6 Redefine spec matrix
  #
  spec <- cbind(1:nrow(x$tt), mMAC, status)
  colnames(spec) <- c("specnb", "mass", "status")
  rownames(spec) <- rownames(x$tt)
  x$spec <- spec
  #
  ## 2.7 2023-07-12 update sort_order
  x$sort_order <- sort_order
  #
  ## 2.8 Define ssnames
  x$ssnames <- DoSSnames(x$tt)
  return(x)
} 