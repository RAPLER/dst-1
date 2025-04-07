createNode <- function(x, q, depth = 0) {
  list(
    x = x,
    value = q,
    depth = depth,
    left = NULL,
    right = NULL
  )
}