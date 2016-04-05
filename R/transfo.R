transfo <- function(zswr, novars,CARD, norel=NULL) {
  # pour transformer les relations originales
  # zswr est une relation décrite selon le format initial
  # zr est la relation représentée dans un espace produit
  # appelle les fonctions PRODUIT et DOUBLES
  # no des variables
  zdims<-matrix(zswr[1,-c(1,2)],ncol = 1)
  novars<-zdims[!duplicated(zdims)] # enlever les doubles
  if (length(novars) < 2) { zr <- zswr} # pas de transfo si 1 var seulement
    else {
    z1 <- productSpace(zswr, novars, CARD) # représentation dans l'espace produit
    # vecteur des masses des ss-ensembles, sans les doubles
    v <-zswr[-1,c(1,2)] 
    v <- v[!duplicated(v[,1]),2]
    # test
    # order vector in decreasing order of the lines, but last element
  #  v<-c(v[(length(v)-1):1], v[length(v)])
    # fin test
    colnz1 <-colnames(z1)
    if (missing(norel)) 
    { norel <- 0 }
    z1 <-bca(v,z1,colnz1, norel)
  }
}