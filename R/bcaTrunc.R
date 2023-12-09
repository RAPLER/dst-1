#'  Truncation of a basic chance assignment mass function
#' 
#'When working with large frames of discernment, the bca resulting of repeated application of Dempster's Rule of Combination can become big. One way to handle this situation could be to group subsets whose mass is less than a small treshold value. The function \code{bcaTrunc} serves this purpose to reduce a large bca to its main elements.\cr
#' @param x A bca to truncate.
#' @param seuil A treshold value
#' @param use_ssnames Put TRUE to use ssnames parameteer instead of description matrix. Default = FALSE.
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
bcaTrunc <-function(x, seuil, use_ssnames = FALSE) {
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
  # test to finish
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
  # 2. Calculations with tt matrix
  if (use_ssnames == FALSE) {  
    # 2.1. The data
    if (isS4(x$tt) == TRUE ) {
      zx <- as.matrix(x$tt) # convert sparse to matrix. to be able t
    } else {
      zx <- x$tt
    }
    zdata<- (cbind(zx, x$spec[,2]) )  # keep the link between focal elements and their spepcs (mass and number).
    # remove frame temporarily (for the case where m(frame) < seuil)
    zdata1 <- zdata[1:(-1+nrow(zdata)),] 
    # remove empty set temporarily if there (for the case where m(empty) < seuil)
    if (sum(zx[1,]) == 0) {
      zdata1 <- zdata1[2:nrow(zdata1),] 
    }
    #
    # 2.2. find rows to merge and do union of these rows
    # Note: IF there is only one subset with mass < treshold, there will be nothing to merge. The bca will remain unchanged.
    in_ztgo <- zdata1[,(1+ncol(zx))] <= seuil # index of elements to group
    # subsetting rows to merge and mass to add
    ztgo <- zdata1[in_ztgo,]  
    # ensure ztgo is always matrix
    if (is.matrix(ztgo) == FALSE) {
      ztgo <- t(as.matrix(ztgo))
    }
    mtoadd <- sum(ztgo[,ncol(ztgo)])
    zb= ztgo[,1:ncol(zx)] # Boolean part of ztgo
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
    x2 <- matrix(c(ztgo_or,mtoadd), nrow = 1, ncol = 1+ ncol(zx), byrow = TRUE)
    if (is.matrix(x2) == FALSE) {
      x2 <- t(as.matrix(x2))
    }
    #
    # 2.3.3 Retain the frame
    x3 <- matrix(zdata[nrow(zdata),], ncol=ncol(zdata) )
    #
    # 2.3.4 Add empty set if present
    if (sum(zx[1,]) == 0) {
      x0 <- matrix(zdata[1,], ncol = ncol(zdata))
    }
    
    # 2.4 Bind the components of the new tt matrix, with their mass
    #
    # 2.4.1 Check if if new subset is the frame. If so ignore the subset and add the mass to the frame
    if (sum(x2[,(-1+1:ncol(x2) )]) == ncol(zx)) {
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
    if (sum(zx[1,]) == 0) {
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
    # vacuous <- bca(matrix(rep(1, ncol(zx)), nrow = 1), m = 1, cnames = colnames(zx))
    # tr_x <-dsrwon(tr_x, vacuous)
    # 3. Construction of the result
    #
    y<-list(con = x$con, tt = tr_x, ssnames = x$ssnames, spec = spec , infovar = x$infovar, varnames = x$varnames, valuenames = x$valuenames, inforel = x$inforel) 
    class(y) <- append(class(y), "bcaspec")
    return(y)
  }
  if (use_ssnames == TRUE) {
    # calculations with ssnames
    # 3.1 The data
    # Remove frame temporarily (for the case where m(frame) < seuil)
    zframe <- unlist((x$ssnames)[shape(x$ssnames)])
    zdata <- x$ssnames
    zmass <- x$spec[,2]
    zdata1 <- (x$ssnames)[-shape(x$ssnames)]
    zmass1 <- (x$spec[,2])[-shape(x$spec[,2])]
    #
    # Initialize list of names
    znames <- list()
    znewmass <- vector()
    #
    # 3.1 Remove empty set temporarily if there (for the case where m(empty) < seuil)
    if (ifelse(length(zdata1[[1]])==1, ifelse(zdata1[[1]] == "Empty", TRUE, FALSE), FALSE)) {
      znames[[1+length(znames)]] <- zdata1[[1]]
      znewmass <- c(znewmass, zmass1[1])
      zdata1 <- zdata1[2:shape(zdata1)] 
      zmass1 <- zmass1[2:shape(zmass1)]
    }
    #
    # 3.2 Retain subsets with mass > seuil
    in_ztokeep <- zmass1 > seuil # index of elements to group
    if (any(in_ztokeep)) {
      ztokeep <- zdata1[in_ztokeep]
      mtkeep <- zmass1[in_ztokeep]
      for (i in 1:shape(ztokeep)) {
        len <- shape(znames)
        znames[[1+len]] <- ztokeep[[i]]
      }
      znewmass <- c(znewmass, mtkeep)
    } else {
      error("seuil need to be lower than the maximum mass")
    }
    #
    # 3.3. find ssnames to merge and do union of these ssnames
    # Note: If there is only one subset with mass < treshold, there will be nothing to merge. The bca will remain unchanged.
    in_ztgo <- zmass1 <= seuil # index of elements to group
    if (any(in_ztgo)) {
      # Union of ssnames
      ztgo <- Reduce("union", zdata1[in_ztgo] )
      # Check if if the new subset is the frame. If so ignore the subset and add its mass to the frame
      if (shape(ztgo) == shape(zframe)) {
        znames[[1+length(znames)]] <- zframe
        mtoadd <- sum(zmass1[in_ztgo])
        znewmass <- c(znewmass, zmass[shape(zmass)] + mtoadd)
      } else {
        znames[[1+length(znames)]] <- ztgo
        # Order elements 
        # addition of masses
        mtoadd <- sum(zmass1[in_ztgo])
        znewmass <- c(znewmass, mtoadd)
        znames[[1+length(znames)]] <- zframe
        znewmass <- c(znewmass, zmass[shape(zmass)] )  
    }
    #
    # 3.4 define the new spec parameter
    zspec <- cbind(1:shape(znewmass), znewmass)
    colnames(zspec) <- c("specnb", "mass")
    } else {
      znames <- x$ssnames
      zspec <- x$spec
    }
    #
    # 3.5 Make the list of ssnames with their mass
    y<-list(con = x$con, tt = NULL, ssnames = znames, spec = zspec , infovar = x$infovar, varnames = x$varnames, valuenames = x$valuenames, inforel = x$inforel) 
    class(y) <- append(class(y), "bcaspec")
    return(y)
  }
}