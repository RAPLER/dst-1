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
  tree <- buildTree(x,m)
  treex <- buildTreeFast(methods::as(x, "RsparseMatrix"),m)
  
  xx <- c(0,1,0)
  s <- c(0,1,0)
  
  tree <- updateTree(tree,as.bit(xx),as.bit(s))
  treex <- updateTreeFast(treex,xx,s)

  q1 <- unravelTree(tree)
  q2 <- unravelTreeFast(treex)
  
  expect_equal(q1,q2)
  
})