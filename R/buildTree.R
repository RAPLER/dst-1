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
buildTree<-function(tt, q){
  # Functions calls: insertNode
  tree <- NULL
  
  n <- if (is.null(nrow(tt))) 1 else nrow(tt)
  
  for (i in 1:n) {
    
    row <- if (is.null(nrow(tt))) tt else tt[i, ]
    
    tree <- insertNode(as.bit(row),q[i],tree)
    
  }
  
  return(tree)
}