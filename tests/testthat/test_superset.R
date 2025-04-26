# Tests "superset" function
context("find the smallest superset")
library(dst)
test_that("superset", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  tree <- buildTree(x,m)
  w1 <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w1)$q,m[3])
  w2 <- as.bit(c(1,1,0))
  expect_equal(superset(tree,w2)$q,m[4])
  w3 <- as.bit(c(0,1,1))
  expect_equal(superset(tree,w3)$q,m[3])
  w4 <- as.bit(c(0,0,0))
  expect_equal(superset(tree,w4)$q,m[1])
  
  # dsrwon test
  to_bitvec <- function(indices, size = 30) {
    v <- rep(FALSE, size)
    v[indices] <- TRUE
    as.integer(v)  # just use integers, no need for as.bit
  }
  
  # build the matrix directly
  tt <- rbind(
    to_bitvec(c(1, 2, 7, 8, 10, 12, 13, 15, 17, 18, 23, 25, 26, 30)),
    to_bitvec(c(1, 2)),
    to_bitvec(c(1)),
    to_bitvec(c(3, 4, 6, 7, 8, 10, 12, 19, 20, 22, 24, 25, 27, 28)),
    to_bitvec(c(1, 3, 4, 7, 8, 12, 16, 23, 24, 25, 26, 27, 28, 30))
  )
  qq <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  tree<-buildTree(tt,qq)
  z1<-to_bitvec(c(12,13))
  expect_equal(superset(tree,z1)$q, qq[1])
  z2<-to_bitvec(c(1,2))
  expect_equal(superset(tree,z2)$q, qq[2])
  
  # TODO: add more tests
})

