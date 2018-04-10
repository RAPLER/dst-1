#' Product space representation of a relation
#'
#' This utility function takes the input matrix of a relation between two or more variables and yields its product space representation. 
#' @param tt A (0,1) or boolean matrix, where the values of the variables put in relation are set side by side, as in a truth table.
#' @param specnb A vector of integers ranging from 1 to \code{k}, where \code{k} is the number of subsets of the \code{tt} matrix. Values must start at one and can be increased only by 0 or 1. They determine the partitioning of the rows of the \code{tt} matrix between the \code{k} subsets.
#' @param infovar  A two-column matrix containing identification numbers of the variables and the number of elements of each variable (size of the frame).
#' @return The matrix of the product space representation of the relation. 
#' @author Claude Boivin, Stat.ASSQ
#' @examples 
#'  ttfw= matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3,
#'   byrow = TRUE, 
#'   dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
#'  specfw = c(1,1,2) 
#'  infovarfw =matrix(c(5,7,2,2), ncol = 2, 
#'  dimnames = list(NULL, c("varnb", "size")) )
#' productSpace(tt=ttfw, specnb=specfw, infovar=infovarfw)
#' @export
#' 
productSpace <- function(tt, specnb, infovar) {
  if (is.matrix(tt) ==FALSE) {
    stop("tt parameter must be a matrix.")
  }
  if ((is.matrix(infovar) ==FALSE) | (sum(infovar[,2]) != ncol(tt))) {
    stop("infovar parameter must be a 2 column matrix with sum of 2nd column = ncol(tt).")
  } else {
  varnb <- (infovar)[,1]
  size <- (infovar)[,2]
  }
  if ((is.numeric(specnb) ==FALSE) |(length(specnb) != nrow(tt))) {
    stop("specnb parameter must be a numeric vector of length nrow(tt)")
  } 
  z1=specnb[-1]
  z0=specnb[-length(specnb)]
  if (sum((z1 - z0) > 1) >0) {
    stop("specnb values must be a sequence of numbers increasing by increments of 1 at most.")
    } else # ok to execute function
    {
  zz=cbind(specnb,tt)
  zz<-as.data.frame(zz)
  znelem <- table(specnb) # nb elements of each specification
  ndims <-length(size)
  zinds <-cumsum(size)
  indinf <- 1+zinds[length(zinds)-1]
  indsup <- zinds[length(zinds)]
  y<-vector()
  #
  # Prepare elements's names as row and column names of the result 
  # in decreasing order
  # use column names
  if (is.null(colnames(tt))) {
    cnames <- paste(rep("c",ncol(tt)),c(1:ncol(tt)),sep="")
  } else {
    cnames <- colnames(tt)
  }
  zNcols <- list(cnames[indinf:indsup]) # pour dimnames de zt
  zNcolsLast <-t(matrix(cnames[indinf:indsup])) # pour dotprod des noms
  if (length(zinds) > 2) {
  for (i in (length(zinds)-1):2) {
    ci <-cnames[(1+zinds[i-1]):(zinds[i])]
    zNcols[[length(zNcols)+1]] <- ci # pour dimnames de zt
    zNcolsLast <- dotprod(matrix(ci), zNcolsLast, "paste", "paste")  # pour dotprod des noms
  zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
    }
  }
  c1 <-cnames[1:zinds[1]]
  zNcols[[length(zNcols)+1]] <- c1 # pour dimnames de zt
  zNcolsLast <- dotprod(matrix(c1), zNcolsLast, "paste", "paste")
  zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
  # End columns names preparation
  #
  # A: loop on the number of subsets
  for (j in 1:max(specnb)) {
    # dimension of result in the product space
    zt<-array(0,dim = size[order(varnb,decreasing = TRUE)], dimnames = zNcols) 
    # B: Loop on the number of elements of the subset
    # B1: working table to gather all the elements of a subset
    zx <- subset(zz, zz$specnb == j)
    zx <-zx[,-1]
    colnames(zx) <- cnames # keep original names if there are duplicates names
    #  B2: Loop on elements of the subset (specification j)
    for (k in 1:znelem[j]) {
      zs <- zx[k,1:zinds[1]]
      zs1=as.vector(t(zs))
      names(zs1) = colnames(zs)
      # C:  Loop on the variables
      for (l in 2:length(varnb)) {
        zw <-zx[k, (1+zinds[l-1]):(zinds[l])]
        zw1=as.vector(t(zw))
        names(zw1) = colnames(zw)
        # elements in the product space
       zs1 <- outer(zw1, zs1, "*")  
      }
      zt <-  zt | zs1 # zt ok, checked
    }
    if (ndims < 3) {
    y <-c(y, zt) # transpose not necessary
    } else {
    y <-c(y, aperm(zt, c(2,1, (3:ndims))))  # à revoir
    }
  } 
    y <-matrix(y, ncol = prod(size), byrow = TRUE) # by rows to follow the order of the column names
    colnames(y) <- zNcolsLast
    y
  }
}