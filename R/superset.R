#' Find the q value of the smallest superset of a vector
#' 
superset<-function(node,w) {
  if (all((node$x & w) == w) && !is.null(node$q)) {
    # return binary vector
    return(node$q)
  }
  
  if (!any(node$x & w)) {
    # move to the left
    return(superset(node$left, w))
  } else {
    # move to the right
    return(superset(node$right, w))
  }
}