# Tests "bca" function
test_that("bcaspec_1", {
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m<- c(.9,.1)
  cnames <- c("yes","no")
  result <- bca(f, m)
  expect_is(result, "bcaspec")
  expect_equal(length(result$infovaluenames$v1), ncol(result$tt))  #  length of value names vector match with input table
})
test_that("bcaspec_2", {
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m = c(0.3, 0.7)
  cnames <- c("yes","no")
 result <- bca(f, m, cnames, infovarnames = c("burglary", "boom") )
  expect_message(bca(f, m, cnames, infovarnames = c("burglary", "boom")) , info = "infovarnames: only the first element kept.")
})