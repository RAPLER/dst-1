updateTrees <- function(node, xx, s, root = node, trees, card_nodup) {
  if (!is.null(node)) {
    
    if (!is.null(node$q)) {
      y <- node$x
      k <- card_nodup[card_nodup >= sum(y | xx)]
      if (length(k) > 0) {
        start <- which(card_nodup == min(k))[1]
        e <- NULL
        for (j in start:length(card_nodup)) {
          e <- superset(trees[[j]], y | xx)
          if (!is.null(e)) {
            break
          }
        }
        
        if (!is.null(e)) {
          z <- e$x
          m0w <- e$q
          
          if (!all(z == y) && all(z == ((y | s) & z))) {
            node$q <- node$q - m0w
          }
        }
      }
    }
    
    node$left <- updateTrees(node$left, xx, s, root, trees, card_nodup)
    node$right <- updateTrees(node$right, xx, s, root, trees, card_nodup)
    
    # Handle empty_set if present
    if (!is.null(node$empty_set)) {
      node$empty_set <- updateTrees(node$empty_set, xx, s, root, trees, card_nodup)
    }
  }
  
  return(node)
}
