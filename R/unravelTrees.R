unravelTrees <- function(trees) {
  values <- list()
  
  # Recursive collection for a single tree
  traverse <- function(node) {
    if (is.null(node)) return()
    
    traverse(node$left)
    
    if (!is.null(node$q) && !is.null(node$index)) {
      values[[length(values) + 1]] <<- list(index = node$index, q = node$q)
    }
    
    traverse(node$right)
    
    # Also traverse the empty set node if it exists
    if (!is.null(node$empty_set)) {
      traverse(node$empty_set)
    }
  }
  
  # Traverse all trees (excluding card_nodup)
  for (tree in trees[seq_along(trees) - 1]) { 
    traverse(tree)
  }
  
  # Sort by index and extract q values
  sorted <- values[order(sapply(values, `[[`, "index"))]
  qq <- sapply(sorted, `[[`, "q")
  
  return(qq)
}
