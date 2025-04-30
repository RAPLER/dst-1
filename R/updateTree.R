updateTree <- function(node, xx, s, root = node) {
  
  if (!is.null(node)) {
    
    if (!is.null(node$q)) {
      y <- node$x
      e <- superset(root, y | xx)  # Use global root here
      if (!is.null(e)) {
        z <- e$x
        m0w <- e$q
        if (!all(z == y) && all(z == ((y | s) & z))) {
          if(all(y==as.bit(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))&&
                 all(z==as.bit(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0)))){
            print("update")
            print(as.integer(y))
            print(as.integer(z))
            print(as.integer(s))
            print(node$q)
            print(m0w)
          }
          node$q <- node$q - m0w
          if(all(y==as.bit(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))&&
             all(z==as.bit(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0)))){
            print(node$q) 
          }
        }
      }
    }
    
    node$left <- updateTree(node$left, xx, s, root)
    node$right <- updateTree(node$right, xx, s, root)
    
    # Also update the empty set node if it exists
    if (!is.null(node$empty_set)) {
      node$empty_set <- updateTree(node$empty_set, xx, s, root)
    }
  }
  
  return(node)
}