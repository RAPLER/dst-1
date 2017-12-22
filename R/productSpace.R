#' Transformation of multiple side inputs to a product space representation
#'
#' This utility function is used to obtain a product space representation of two or more variables initially represented in a truth table format.
#' @param x The input variables in the following format: \itemize{
#'  \item tt The table of the variables being put in relation. This relation is described by a matrix input table of (0,1), where the (0,1) values of all variables are side by side, as in a truth table.
#'  \item spec A two column matrix. First column contains specification numbers. Second column (not used) contains the mass associated with each element of the relation.
#'  \item infovar  A two column matrix containing identification numbers of the variables and the number of elements of each variable.
#' }
#' @return The product space representation of the input variables
#' @author Claude Boivin, Stat.ASSQ
#' @examples 
#'  ttfw= matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
#'  specfw = matrix(c(1,1,2,0.8,0.8,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#'  infovarfw =matrix(c(5,7,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#'  x <- list(tt=ttfw, spec=specfw, infovar=infovarfw)
#' y <- productSpace(x)
#' @export
#' 
productSpace <- function(x) {
  if (is.list(x) == FALSE) {
    stop("Input is not a list.")
  }
  att <- (attributes(x))$names
  if (("infovar" %in% att) == FALSE) {
    stop("List member infovar missing.")
  } else {
    varnb <- (x$infovar)[,1]
    size <- (x$infovar)[,2]
  }
  specnb <- (x$spec)[,1]
 #  mass <- (x$spec)[,2]  # not used
  zz=cbind(specnb,x$tt)
  zz<-as.data.frame(zz)
  znelem <- table(specnb) # nb elements of each specification
  ndims <-length(size)
  zinds <-cumsum(size)
  indinf <- 1+zinds[length(zinds)-1]
  indsup <- zinds[length(zinds)]
  y<-vector()
  #
  # Prepare elements's names as row and column names of the result 
  # on va en ordre décroissant
  colsx <- colnames(x$tt)
#  colsx <- colsx[-c(1,2)] # pour conserver les noms dupliqués si on en a 
  indinf <- 1+zinds[length(zinds)-1]
  indsup <- zinds[length(zinds)]
  zNcols <- list(colsx[indinf:indsup]) # pour dimnames de zt
  zNcolsLast <-t(matrix(colsx[indinf:indsup])) # pour dotprod des noms
  if (length(zinds) > 2) {
  for (i in (length(zinds)-1):2) {
    ci <-colsx[(1+zinds[i-1]):(zinds[i])]
    zNcols[[length(zNcols)+1]] <- ci # pour dimnames de zt
    zNcolsLast <- dotprod(matrix(ci), zNcolsLast, "paste", "paste")  # pour dotprod des noms
  zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
  }
  }
  c1 <-colsx[1:zinds[1]]
  zNcols[[length(zNcols)+1]] <- c1 # pour dimnames de zt
  zNcolsLast <- dotprod(matrix(c1), zNcolsLast, "paste", "paste")
  zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
  #
  # A: boucle sur le nombre de ss-ensembles
  for (j in 1:max(specnb)) {
    # dimension des résultats dans l'espace produit
    zt<-array(0,dim = size[order(varnb,decreasing = TRUE)], dimnames = zNcols) 
    # tableau de travail pour réunir les éléments d'un ss-ensemble
    zx <- subset(zz, zz$specnb == j)
    zx <-zx[,-1]
    colnames(zx)= colsx # conserver les noms originaux si on a des noms dupliqués
    # B:  Boucle sur le nombre d'éléments du ss-ensemble
    for (k in 1:znelem[j]) {
      zs <- zx[k,1:zinds[1]]
      zs1=as.vector(t(zs))
      names(zs1) = colnames(zs)
      # C:  boucle sur le nombre de variables
 #     colnV1 <- colsx[1:zinds[1]]
      for (l in 2:length(varnb)) {
        zw <-zx[k, (1+zinds[l-1]):(zinds[l])]
        zw1=as.vector(t(zw))
        names(zw1) = colnames(zw)
        # éléments dans l'espace produit
        ## test
       zs1 <- outer(zw1, zs1, "*")  
#        zs1 <- outer(zs1, zw1, "*")  marche pas avec 3 variables
      ## fin test  
      }
      zt <-  zt | zs1 # zt ok, checked
    }
    # test
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