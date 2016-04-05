productSpace <- function(zswr, novars, CARD) {
  library(dplyr)
  zz<-as.data.frame(zswr)
  znelem <- table(zz[-1,1]) # nb elements of each subset
  ndims <-length(CARD)
  zinds <-cumsum(CARD)
  indinf <- 1+zinds[length(zinds)-1]
  indsup <- zinds[length(zinds)]
  zr<-vector()
  #
  # Prepare elements's names as row and column names of the result 
  # on va en ordre décroissant
  colsswr <- colnames(zswr)
  colsswr <- colsswr[-c(1,2)] # pour conserver les noms dupliqués si on en a 
  indinf <- 1+zinds[length(zinds)-1]
  indsup <- zinds[length(zinds)]
  zNcols <- list(colsswr[indinf:indsup]) # pour dimnames de zt
  zNcolsLast <-t(matrix(colsswr[indinf:indsup])) # pour dotprod des noms
  if (length(zinds) > 2) {
  for (i in (length(zinds)-1):2) {
    ci <-colsswr[(1+zinds[i-1]):(zinds[i])]
    zNcols[[length(zNcols)+1]] <- ci # pour dimnames de zt
    zNcolsLast <- dotprod(matrix(ci), zNcolsLast, "paste", "paste")  # pour dotprod des noms
    # test pas de transposée pour l'ordre des vars
     zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
  #  zNcolsLast <-matrix((zNcolsLast), ncol = prod(dim(zNcolsLast)))
    # fin test
  }
  }
  c1 <-colsswr[1:zinds[1]]
  zNcols[[length(zNcols)+1]] <- c1 # pour dimnames de zt
  zNcolsLast <- dotprod(matrix(c1), zNcolsLast, "paste", "paste")
  # test pas de transposée pour l'ordre des vars
   zNcolsLast <-matrix(t(zNcolsLast), ncol = prod(dim(zNcolsLast)))
#  zNcolsLast <-matrix((zNcolsLast), ncol = prod(dim(zNcolsLast)))
  # fin tetst
  ##
  # A: boucle sur le nombre de ss-ensembles
  for (j in 1:max(zz[,1])) {
    # dimension des résultats dans l'espace produit
    zt<-array(0,dim = CARD[order(novars,decreasing = TRUE)], dimnames = zNcols) 
    # tableau de travail pour réunir les éléments d'un ss-ensemble
    zx <- subset(zz, zz$nospec == j)
    zx <-zx[,-c(1,2)]
    colnames(zx)= colsswr # conserver les noms originaux si on a des noms dupliqués
    # B:  Boucle sur le nombre d'éléments du ss-ensemble
    for (k in 1:znelem[j]) {
      zs <- zx[k,1:zinds[1]]
      zs1=as.vector(t(zs))
      names(zs1) = colnames(zs)
      # C:  boucle sur le nombre de variables
 #     colnV1 <- colsswr[1:zinds[1]]
      for (l in 2:length(novars)) {
        zw <-zx[k, (1+zinds[l-1]):(zinds[l])]
        zw1=as.vector(t(zw))
        names(zw1) = colnames(zw)
        # éléments dans l'espace produit
        zs1 <- outer(zw1, zs1, "*")  
      }
      zt <-  zt | zs1 # zt ok, checked
    }
    if (ndims < 3) {
      ##
      # test
   # zr <-c(zr, t(zt)) # transpose for destructuring by lines
      zr <-c(zr, zt)
    # fin test
      ##
    } else {
      ##
      # test
    # zr <-c(zr, aperm(zt, c(2,1, (3:ndims))))  # à revoir
      zr <-c(zr, zt)
      # fin test
      ##
    }
  } 
    zr <-matrix(zr, ncol = prod(CARD), byrow = TRUE)
    colnames(zr) <- zNcolsLast
    zr
}