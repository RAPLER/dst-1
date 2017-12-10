# Tests "bca" function for errors
test_that("bcaspec_err1", {
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m<- c(.9,.1)
  cnames <- c("yes","no")
  result <- bca(f, m, cnames )
  compare(colnames(result$tt), cnames)
  expect_error(bca(f, m, cnames = c("a", "b", "c") ) , "Error in input arguments: check your input data.")
})
test_that("bcaspec_err2", {
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m = c(0.1, 0.2, 0.7)
  cnames <- c("yes","no")
  expect_error(bca(f,  m = c(0.1, 0.2, 0.7), cnames ) , "Error in input arguments: check your input data.")
})
test_that("bcaspec_err3", {
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m = c(0.3, 0.8)
  cnames <- c("yes","no")
  expect_error(bca(f,  m = c(0.3, 0.8), cnames ) , "Error in input arguments: check your input data.")
})