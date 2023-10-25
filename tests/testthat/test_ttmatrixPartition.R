context("create partition matrix")
library(dst)
test_that("partition", {
  # test dimensions
  x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
  pa <- ttmatrixPartition(3, x$infovar[2])
  expect_equal(dim(pa), c(3,3))
})
