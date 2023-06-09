#'  Truncation of a basic chance assignment mass function
#' 
#'When working with large frames of discernment, the bca resulting of repeated application of Dempster's Rule of Combination can become big. One way to handle this situation could be to group subsets whose mass is less than a small treshold value. The function \code{bcaTrunc} serves this purpose to reduce a large bca to its main elements.\cr
#' @param x A bca to truncate.
#' @param seuil A treshold value
#' @return tr_x The bca object truncated.
#' @author Claude Boivin
#' @export
#' @examples 
#'x <- bca(tt = matrix(c(0,1,0,0, 
#'0,0,1,1,
#'1,1,0,0,
#'1,0,1,0,
#'0,1,1,0,
#'1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17),
#'cnames = c("a", "b", "c", "d"))
#'bcaPrint(x)
#'tr_x <- bcaTrunc(x, seuil = 0.1)
#'bcaPrint(tr_x)
#' 
bcaTrunc <-function(x, seuil) {
  #
  # Local variables: zdata, in_ztgo, ztgo, zl, ztgo_or, zz, x1, tr_x, 
  # Functions calls: reduction, bca, dsrwon
  #
  ## 1. Checks
  # 1.1. class bcaspec
  #
  if (inherits(x, "bcaspec") == FALSE) {
    stop("Input not of class bcaspec.")
  }
  #
  # 1.2. Check treshold value
  if( (is.numeric(seuil) == FALSE) | (seuil >=1) | (seuil <= 0) ) { 
    stop("Treshold must be a numeric value between 0 and 1.") 
  }
  #
  # 2. Calculations
  # 2.1. The data
  zdata<- (cbind(x$tt, x$spec) )  # keep the link between focal elements and their spepcs (mass and number).
  zdata <- zdata[1:(-1+nrow(zdata)),] # remove frame temporarily (for the case where m(frame) < seuil)
  #
  # 2.2. find rows to merge and do union of these rows
  # Note: IF there is only one subset with mass < treshold, there will be nothong to merge. The bca will remain unchanged.
  in_ztgo <- zdata[,(2+ncol(x$tt))] < seuil # index of elements to group
  ztgo <- zdata[in_ztgo,]  # subsetting rows to merge
  # ensure ztgo is always matrix
  if (is.matrix(ztgo) == FALSE) {
    ztgo <- t(as.matrix(ztgo))
  }
  zb= ztgo[,1:ncol(x$tt)]
  # ensure zb is always matrix
  if (is.matrix(zb) == FALSE) {
    zb <- t(as.matrix(zb))
  }
  # ztgo_or <- apply(ztgo[,1:ncol(x$tt)], 2, any) # obtain union of subsets 
  ztgo_or=apply(zb, 2, FUN= function(zb) {reduction(zb, f="|") } )  # Union of subsets
  zz <- matrix(ztgo_or, nrow = nrow(ztgo), ncol = ncol(x$tt), byrow = TRUE) # rows to group are described by the same subset (ztgo_or)
  #
  # 2.3.  modify the tt matrix of the bca
  x1=x$tt
  x1[in_ztgo*(1:(-1+nrow(x$tt)) ) ,] <- zz # Modification de la matrice tt
  tr_x <- x
  tr_x$tt <- x1
  #
  # 2.4 Use dsrwon to add the mass of grouped rows
  vacuous <- bca(matrix(rep(1, ncol(x$tt)), nrow = 1), m = 1, cnames = colnames(x$tt))
  tr_x <-dsrwon(tr_x, vacuous)
  return(tr_x)
 }