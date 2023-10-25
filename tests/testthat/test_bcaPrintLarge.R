context("Print summary statistics of large mass functions")
library(dst)
library(tidyverse)
test_that("bcaPrintLarge", {
  # empty test
  x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
  bcaPrintLarge(x)
})
