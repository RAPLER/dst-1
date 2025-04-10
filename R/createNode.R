#' Change depth to appropriate calculation
#' - depth equals the greatest index corresponding to 1 in the bit vector x
createNode<-function(x, q) {
  d <- if(any(x)) max(which(x==1)) - 1 else -1
  list(
    x = x,
    q = q,
    left = NULL,
    right = NULL,
    depth = d
  )
}