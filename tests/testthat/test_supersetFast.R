# Tests "supersetFast" function
context("find the smallest supersetFast")
library(dst)
test_that("supersetFastFast", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  tree <- buildTreeFast(methods::as(x, "RsparseMatrix"),m)
  w1 <- c(0,1,0)
  expect_equal(inspectNode(supersetFast(tree,w1))$q,m[3])
  w2 <- c(1,1,0)
  expect_equal(inspectNode(supersetFast(tree,w2))$q,m[4])
  # same
  w3 <- c(0,1,1)
  expect_equal(inspectNode(supersetFast(tree,w3))$q,m[3])
  w4 <- c(0,0,0)
  expect_equal(inspectNode(supersetFast(tree,w4))$q,m[1])
  
  # Test Fig 12 + b
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,1,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.3,0.1)
  tree <- buildTreeFast(methods::as(x, "RsparseMatrix"),m)
  w1 <- c(0,1,0)
  expect_equal(inspectNode(supersetFast(tree,w1))$q,m[5])
  w2 <- c(1,1,0)
  expect_equal(inspectNode(supersetFast(tree,w2))$q,m[4])
  # same
  w3 <- c(0,1,1)
  expect_equal(inspectNode(supersetFast(tree,w3))$q,m[3])
  w4 <- c(0,0,0)
  expect_equal(inspectNode(supersetFast(tree,w4))$q,m[1])
  
  # Test Fig 12 + emptyset
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,0,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.3,0.1)
  tree <- buildTreeFast(methods::as(x, "RsparseMatrix"),m)
  w1 <- c(0,1,0)
  expect_equal(inspectNode(supersetFast(tree,w1))$q,m[3])
  w2 <- c(1,1,0)
  expect_equal(inspectNode(supersetFast(tree,w2))$q,m[4])
  # same
  w3 <- c(0,1,1)
  expect_equal(inspectNode(supersetFast(tree,w3))$q,m[3])
  w4 <- c(0,0,0)
  expect_equal(inspectNode(supersetFast(tree,w4))$q,m[5])
  
  # Test simple support
  x <- matrix(c(0,1,1,
                1,1,0,
                0,1,0,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTreeFast(methods::as(x, "RsparseMatrix"),m)
  
  w1 <- c(0,1,0)
  expect_equal(inspectNode(supersetFast(tree,w1))$q,q[3])
  w2 <- c(1,1,0)
  expect_equal(inspectNode(supersetFast(tree,w2))$q,q[2])
  # same
  w3 <- c(0,1,1)
  expect_equal(inspectNode(supersetFast(tree,w3))$q,q[1])
  w4 <- c(0,0,0)
  expect_equal(inspectNode(supersetFast(tree,w4))$q,q[3])
  w5 <- c(1,1,1)
  expect_equal(inspectNode(supersetFast(tree,w5))$q,q[4])
  
  # TODO: add more tests
})

