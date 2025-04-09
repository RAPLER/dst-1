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
  
  # an example from dsrwon test
  tt1 <- matrix(c(
    0,0,1,1,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,
    0,0,1,1,1,0,1,1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,0,1,0,1,1,1,0,0,
    1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  ), nrow = 4, byrow = TRUE)
  
  rownames(tt1) <- c(
    "3 + 4 + 7 + 8 + 12 + 24 + 26 + 27 + 28",
    "3 + 4 + 5 + 7 + 8 + 10 + 12 + 14 + 15 + 18 + 20 + 22 + 24 + 26 + 27 + 28",
    "1 + 3 + 4 + 7 + 8 + 9 + 12 + 16 + 17 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30",
    "frame"
  )
  
  q1 <- c(1.0000, 0.9900, 0.9900, 0.9801)
  names(q1) <- rownames(tt1)
  
  # TODO: make sure this transplant is correct
  tree1 <- buildTree(tt1,q1)
  
  # TODO: add more tests
  
})
