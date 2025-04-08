# Tests "superset" function
context("find the smallest superset")
library(dst)
test_that("superset", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  q <- commonality(x,m,"ezt-m")
  tree <- buildTree(x,q)
  w <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w),0.7)
  # TODO: add more tests
})