unravelTree<-function(tree) {
  values <- list()
  
  # Recursive collection
  traverse <- function(node) {
    if (is.null(node)) return()
    
    traverse(node$left)
    
    if (!is.null(node$q) && !is.null(node$index)) {
      values[[length(values) + 1]] <<- list(index = node$index, q = node$q)
    }
    
    traverse(node$right)
  }
  
  traverse(tree)
  
  # Sort by index and extract q values
  sorted <- values[order(sapply(values, `[[`, "index"))]
  qq <- sapply(sorted, `[[`, "q")
  
  return(qq)
}