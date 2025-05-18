# Tests "updateTrees" function
context("update a list of trees")
library(dst)
test_that("updateTree", {
  # 
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  qq <- commonality(x,m,"ezt-m")
  tree <- buildTree(x,qq)
  treex <- buildTreeFast(methods::as(x, "RsparseMatrix"),qq)
  
  xx <- c(0,1,0)
  s <- c(1,1,1)
  
  tree <- updateTree(tree,as.bit(xx),as.bit(s))
  treex <- updateTreeFast(treex,xx,s)

  q1 <- unravelTree(tree)
  q2 <- unravelTreeFast(treex)
  
  expect_equal(q1,q2)
  
})