#' Find the q value of the smallest superset of a vector
#' 
superset<-function(node,w) {
  if (all((node$x & w) == w) && !is.null(node$q)) {
    # return q value
    return(node$q)
  }
  
  if (!all(node$x[0:node$depth] == w[0:node$depth])) {
    # give up this branch
    return(NULL)
  } else if (w[node$depth+1]) {
    # move to the right
    return(superset(node$right, w))
  } else {
    # move to the left
    return(superset(node$left, w))
  }
}