# Tests "buildTree" function
context("build a tree")
library(dst)
test_that("buildTree", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  
  q <- commonality(x,m,"ezt-m")
  tree <- buildTree(x,q)
  
  expect_equal(tree$x,as.bit(x[1,]))
  expect_equal(tree$q,q[1])
  expect_equal(tree$depth,0)
  
  expect_equal(tree$left$x,as.bit(c(0,1,0)))
  expect_equal(tree$left$q,NULL)
  expect_equal(tree$left$depth,1)
  
  expect_equal(tree$right$x,as.bit(x[4,]))
  expect_equal(tree$right$q,q[4])
  expect_equal(tree$right$depth,2)
  
  expect_equal(tree$left$left$x,as.bit(x[2,]))
  expect_equal(tree$left$left$q,q[2])
  expect_equal(tree$left$left$depth,2)
  
  expect_equal(tree$left$right$x,as.bit(x[3,]))
  expect_equal(tree$left$right$q,q[3])
  expect_equal(tree$left$right$depth,2)
  
  # Test Fig 12 + b
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,1,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  # TODO: add more tests
})
