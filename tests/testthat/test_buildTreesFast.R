# Tests "buildTreesFast" function
context("build a tree")
library(dst)
test_that("buildTreesFast", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  trees <- buildTreesFast(methods::as(x, "RsparseMatrix"),q)
  trees <- inspectNodes(trees)
  
  expect_equal(trees[[1]]$x,unname(x[1,]))
  expect_equal(trees[[1]]$q,q[1])
  expect_equal(trees[[1]]$depth,0)
  
  expect_equal(trees[[1]]$left$x,unname(x[2,]))
  expect_equal(trees[[1]]$left$q,q[2])
  expect_equal(trees[[1]]$left$depth,2)
  
  expect_equal(trees[[2]]$x,unname(x[3,]))
  expect_equal(trees[[2]]$q,q[3])
  expect_equal(trees[[2]]$depth,2)
  
  expect_equal(trees[[3]]$x,unname(x[4,]))
  expect_equal(trees[[3]]$q,q[4])
  expect_equal(trees[[3]]$depth,2)
  
  expect_equal(trees[[4]],c(1,2,3))
  
  # Test Fig 12 + b
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,1,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  trees <- buildTreesFast(methods::as(x, "RsparseMatrix"),q)
  trees <- inspectNodes(trees)
  
  expect_equal(trees[[1]]$left$x,unname(x[5,]))
  expect_equal(trees[[1]]$left$q,q[5])
  expect_equal(trees[[1]]$left$depth,1)
  
  # Test Fig 12 + emptyset
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,0,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  trees <- buildTreesFast(methods::as(x, "RsparseMatrix"),q)
  trees <- inspectNodes(trees)
  
  expect_equal(trees[[1]]$x,unname(x[5,]))
  expect_equal(trees[[1]]$q,q[5])
  expect_equal(trees[[1]]$depth,-1)
  
  # Test Fig 12 + emptyset in the middle
  x <- matrix(c(0,0,1,
                0,0,0,
                1,0,0,
                0,1,1,
                1,1,1), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  trees <- buildTreesFast(methods::as(x, "RsparseMatrix"),q)
  trees <- inspectNodes(trees)
  
  expect_equal(trees[[1]]$x,unname(x[2,]))
  expect_equal(trees[[1]]$q,q[2])
  expect_equal(trees[[1]]$depth,-1)
  
  # Test Fig 12 + emptyset in the first row
  x <- matrix(c(0,0,0,
                0,0,1,
                1,0,0,
                0,1,1,
                1,1,1), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  trees <- buildTreesFast(methods::as(x, "RsparseMatrix"),q)
  trees <- inspectNodes(trees)
  
  expect_equal(trees[[1]]$x,unname(x[1,]))
  expect_equal(trees[[1]]$q,q[1])
  expect_equal(trees[[1]]$depth,-1)
  
  # TODO: add more tests
})
