#' Change depth to appropriate calculation
#' - depth equals the greatest index corresponding to 1 in the bit vector x
createNode<-function(x, q) {
  d <- max(which(x==1))-1
  list(
    x = x,
    value = q,
    left = NULL,
    right = NULL,
    depth = d
  )
}