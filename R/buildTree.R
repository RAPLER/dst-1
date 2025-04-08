#' Build a tree
#'
buildTree<-function(tt, q){
  tree <- NULL
  
  sort_order <-order(apply(tt,1,sum))
  tt <- tt[sort_order,]
  q <- q[sort_order]
  
  for (i in 1:nrow(tt)) {
    tree <- insertNode(as.bit(tt[i,]),q[i],tree)
  }
  
  return(tree)
}