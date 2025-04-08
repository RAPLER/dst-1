buildTree<-function(tt, q){
  tree <- NULL
  # TODO: sort by subset size
  # sort tt by rowsum
  # sort q the above order
  
  for (i in 1:nrow(tt)) {
    tree <- insertNode(tt[i,],q[i],tree)
  }
  
  return(tree)
}