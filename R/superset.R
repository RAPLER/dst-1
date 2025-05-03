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
  if (node$depth>0) {
    if (!all((node$x[1:(node$depth+1)] & w[1:(node$depth+1)]) == w[1:(node$depth+1)])) {
      # give up this branch
      return(NULL)
    }
  }
  if (w[node$depth+1]) {
    # move to the right
    #print("R")
    return(superset(node$right, w))
  } else {
    # move to the left
    #print("L")
    result <- superset(node$left, w)
    if (!is.null(result)) return(result)
    #print("Not L")
    #print("R")
    return(superset(node$right, w))
  }
}