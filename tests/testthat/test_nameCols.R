context("Naming the columns of a matrix")
library(dst)
test_that("nameCols", {
  # T1: input 1 must be a list
  expect_error(nameCols(valuenames = c("a","b","c"), size = matrix(1:6, ncol=2)), "first parameter not a list.")
  #
  # T2: input 2  must be a matrix
  expect_error(nameCols(valuenames = list(x = c("a", "b")), size = c("a","b","c")), "Parameter size must be numeric.")
  #
})