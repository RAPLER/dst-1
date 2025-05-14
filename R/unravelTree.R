unravelTree <- function(tree) {
  values <- list()
  
  # Recursive collection
  traverse <- function(node) {
    if (is.null(node)) return()
    
    traverse(node$left)
    
    if (!is.null(node$q) && !is.null(node$index)) {
      for (idx in node$index) {
        values[[length(values) + 1]] <<- list(index = idx, q = node$q)
      }
    }
    
    traverse(node$right)
    
    if (!is.null(node$empty_set)) {
      traverse(node$empty_set)
    }
  }
  
  traverse(tree)
  
  # Sort by index and extract q values
  sorted <- values[order(sapply(values, `[[`, "index"))]
  qq <- sapply(sorted, `[[`, "q")
  
  return(qq)
}
