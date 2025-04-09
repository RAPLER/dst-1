#' Find the q value of the smallest superset of a vector
#' 
superset<-function(node,w) {
  # TODO: if the tree is correctly built, this is incorrect
  if (all((node$x & w) == w) && !is.null(node$q)) {
    # return binary vector
    return(node$q)
  }
  
  if (all((node$x & w) == w)) {
    # move to the left
    return(superset(node$left, w))
  } else {
    # move to the right
    return(superset(node$right, w))
  }
}