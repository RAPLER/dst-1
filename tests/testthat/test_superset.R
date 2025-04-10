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
  ww <- as.bit(c(1,1,0))
  expect_equal(superset(tree,ww),0.4)
  
  # dsrwon test
  to_bitvec <- function(indices, size = 30) {
    v <- rep(FALSE, size)
    v[indices] <- TRUE
    as.bit(v)
  }
  sets <- list(
    to_bitvec(c(1, 2, 7, 8, 10, 12, 13, 15, 17, 18, 23, 25, 26, 30)),
    to_bitvec(c(1, 2)),
    to_bitvec(c(1)),
    to_bitvec(c(3, 4, 6, 7, 8, 10, 12, 19, 20, 22, 24, 25, 27, 28)),
    to_bitvec(c(1, 3, 4, 7, 8, 12, 16, 23, 24, 25, 26, 27, 28, 30))
  )
  q <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  tree <- NULL
  for (i in order(sapply(sets, function(s) sum(as.integer(s))))) {
    tree <- insertNode(sets[[i]], q[i], tree)
  }
  z<-to_bitvec(c(12,13))
  expect_equal(superset(tree,z), 0.1)
  # TODO: add more tests
})

