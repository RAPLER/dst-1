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
  # 1.3 TO DO: Check that empty set is in first row if present
  # test to finise
  ###
  # sort_order<-order(apply(W1,1,sum))
  # tt <- W1[sort_order,]
  # if (is.matrix(tt) == FALSE) {
  #   tt <- matrix(tt,ncol = length(tt), dimnames = list(NULL, names(tt)))
  # }
  # Identify if the empty set is present and define m_empty accordingly with it mass
  # # Put masses in the same order as the tt matrix
  # #
  # z<- sum(W1[sort_order[1],])
  # if (z==0) {
  #   empty<-sort_order[1]  
  #   m_empty<-MAC[empty] 
  # } else {
  #   empty<-0
  #   m_empty<-0
  # }
  ###
  #
  # 2. Calculations
  # 2.1. The data
  zdata<- (cbind(x$tt, x$spec[,2]) )  # keep the link between focal elements and their spepcs (mass and number).
  # remove frame temporarily (for the case where m(frame) < seuil)
  zdata1 <- zdata[1:(-1+nrow(zdata)),] 
  # remove empty set temporarily if there (for the case where m(empty) < seuil)
  if (sum(x$tt[1,]) == 0) {
    zdata1 <- zdata1[2:nrow(zdata1),] 
  }
  #
  # 2.2. find rows to merge and do union of these rows
  # Note: IF there is only one subset with mass < treshold, there will be nothong to merge. The bca will remain unchanged.
  in_ztgo <- zdata1[,(1+ncol(x$tt))] < seuil # index of elements to group
  # subsetting rows to merge and mass to add
  ztgo <- zdata1[in_ztgo,]  
  # ensure ztgo is always matrix
  if (is.matrix(ztgo) == FALSE) {
    ztgo <- t(as.matrix(ztgo))
  }
  mtoadd <- sum(ztgo[,ncol(ztgo)])
  zb= ztgo[,1:ncol(x$tt)] # Boolean part of ztgo
  # ensure zb is always matrix
  if (is.matrix(zb) == FALSE) {
    zb <- t(as.matrix(zb))
  }
 # disjunction of subsets of ztgo
  ztgo_or=apply(zb, 2, FUN= function(zb) {reduction(zb, f="|") } ) 
  #
  # 2.3 construct new tt matrix and new mass vestor
  #
  # 2.3.1 Retain subsets with mass > seuil
  x1 <- zdata1[!in_ztgo,]
  if (is.matrix(x1) == FALSE) {
    x1 <- t(as.matrix(x1))
  }
  # 2.3.2 Add disjunction of subsets with mass < seuil
  x2 <- matrix(c(ztgo_or,mtoadd), nrow = 1, ncol = 1+ ncol(x$tt), byrow = TRUE)
  if (is.matrix(x2) == FALSE) {
    x2 <- t(as.matrix(x2))
  }
  #
  # 2.3.3 Retain the frame
  x3 <- matrix(zdata[nrow(zdata),], ncol=ncol(zdata) )
  #
  # 2.3.4 Add empty set if present
  if (sum(x$tt[1,]) == 0) {
    x0 <- matrix(zdata[1,], ncol = ncol(zdata))
  }
  
  # 2.4 Bind the components of the new tt matrix, with their mass
  #
  # 2.4.1 Check if if new subset is the frame. If so ignore the subset and add the mass to the frame
  if (sum(x2[,(-1+1:ncol(x2) )]) == ncol(x$tt)) {
    x3[,ncol(zdata)] <- x3[,ncol(zdata)] + mtoadd
    zz <- x3
  } else{
    zz <-rbind(x2, x3)
  }
  #
  # 2.4.2 Add subsets with mass > seuil
  zz <- rbind(x1, zz)
  #
  # 2.4.3 Add empty set if present
  #
  if (sum(x$tt[1,]) == 0) {
  zz <- rbind(x0, zz)
  } 
  #
  tr_x <- zz[,1:(-1+ncol(zz) )]
  rownames(tr_x) <- nameRows(tr_x)
  spec <- cbind((1:nrow(tr_x)), zz[,ncol(zz)])
  rownames(spec) <- rownames(tr_x)
  colnames(spec) <- c("specnb", "mass")
  #
  # #
  # # 2.4 Use dsrwon to add the mass of grouped rows
  # vacuous <- bca(matrix(rep(1, ncol(x$tt)), nrow = 1), m = 1, cnames = colnames(x$tt))
  # tr_x <-dsrwon(tr_x, vacuous)
  # 3. Construction of the result
  #
  y<-list(con = x$con, tt = tr_x,  spec = spec , infovar = x$infovar, varnames = x$varnames, valuenames = x$valuenames, inforel = x$inforel) 
  class(y) <- append(class(y), "bcaspec")
  return(y)
 }