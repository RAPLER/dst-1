createNode <- function(x, q) {
  # TODO: change depth to appropriate calculation
  # TODO: decide depth by x
  # - Depth equals the greatest index corresponding to 1 in the bit vector x
  list(
    x = x,
    value = q,
    left = NULL,
    right = NULL,
    depth = 0
  )
}