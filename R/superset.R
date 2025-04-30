#' Find the q value of the smallest superset of a vector
#' 
superset<-function(node,w) {
  if(is.null(node)) {
    return(NULL)
  }
  if (all((node$x & w) == w) && !is.null(node$q)) {
    # return q value
    return(node)
  } 
  if (node$depth!=0) {
    if (!all((node$x[1:node$depth] & w[1:node$depth]) == w[1:node$depth])) {
      # give up this branch
      return(NULL)
    }
  }
  if (w[node$depth+1]) {
    # move to the right
    return(superset(node$right, w))
  } else {
    # move to the left
    result <- superset(node$left, w)
    if (!is.null(result)) return(result)
    return(superset(node$right, w))
  }
}