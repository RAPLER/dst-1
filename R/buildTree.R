#' Build a tree of...
#' 
#' @details A tree structure of commonalities is built from the binary matrix\code{tt}. This tree structure can then be used with large frames of discernment and optimize the combination with Dempster's rule
#' @param  tt A (0,1)-matrix or a boolean matrix. The number of columns must match the number of elements (values) of the frame of discernment.
#' @param  q Commonality values of the tt matrix.
#' @return The tree struct
#' @author  Peiyuan Zhu
#' @import methods bit
#' @export
#' @references  Chaveroche, Franck Davoine, Véronique Cherfaoui. Eﬃcient Möbius Transformations and their applications to Dempster-Shafer Theory: Clarification and implementation. ArXiv preprint arXiv:2107.07359
#' @examples 
#' # Example from figure 12 of the cited reference.
#'  x <- matrix(c(1,0,0,
#'  0,0,1,
#'  0,1,1,
#'  1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
#'  rownames(x) <- nameRows(x)
#'  m <- c(0.1,0.2,0.3,0.4)
#'  q <- commonality(x,m,"ezt-m")
#'  q_tree <- buildTree(x,q)
buildTree <- function(tt, qq, indices = NULL) {
  tree <- NULL
  empty_set <- FALSE
  
  if (is.null(nrow(tt))) {
    tt <- matrix(tt, nrow = 1)
  }
  
  # Compute depth of each set
  depth <- apply(tt, 1, function(x) if (sum(x) > 0) max(which(x == 1)) - 1 else -1)
  
  # Default to 1...n if indices not provided
  if (is.null(indices)) {
    indices <- seq_len(nrow(tt))
  }
  
  sort_order <- order(depth)
  
  for (k in sort_order) {
    i <- indices[k]
    x <- tt[k, ]
    
    if (sum(x) == 0) {
      q <- qq[i]
      j <- i
      empty_set <- TRUE
      next
    }
    
    node <- createNode(as.bit(x), qq[i], i)
    tree <- insertNode(node, tree)
  }
  
  if (empty_set) {
    tree$empty_set <- createNode(as.bit(rep(0, ncol(tt))), q, j)
  }
  
  return(tree)
}


