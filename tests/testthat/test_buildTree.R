# Tests "buildTree" function
context("build a tree")
library(dst)
test_that("buildTree", {
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  q <- commonality(x,m,"ezt-m")
  tree <- buildTree(x,q)
})