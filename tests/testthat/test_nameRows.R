context("Naming the rows of a matrix")
library(dst)
test_that("nameRows", {
  # T1: input must be a matrix
  expect_error(nameRows(c(1,1,3)), "Input is not a matrix.")
  # expect_error(nameRows(c(1,1,3)), "Input is not a matrix.")
  # T2: test if logical matrix
  f <- matrix(1:3, ncol=3)
  colnames(f)=c("a", "b", "c")
  expect_error(object = nameRows(f), c("Input is not a logical or \\(0,1\\) matrix."))
  # T3: test with no column names
  f1 <- matrix(c(0,0,0,1,0,0,0,0,1,1,0,1,1,1,1),ncol=3, byrow = TRUE)
  expect_warning(nameRows(f1), "No column names supplied. Column names are generated.")
  # T4: check matrix of one row
  f2= matrix(c(1,0,1), ncol=3)
  colnames(f2)=c("a", "b", "c")
  expect_equal(nameRows(f2), c("a + c"))
  # T5: check logical matrix of one row
  f3= matrix(c(1,0,1), ncol=3)> 0
  colnames(f3)=c("a", "b", "c")
  expect_equal(nameRows(f3), c("a + c"))
})