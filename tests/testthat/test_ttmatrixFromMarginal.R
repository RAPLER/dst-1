# Tests "ttmatrixFromMarginal" function
context("construct tt martrix of a bca from marginal probabiltiies")
library(dst)
test_that("test_ttmatrixFromMarginal", {
  p <- c(2,2,1.5,1.2,1,0,0)
  tt <- ttmatrixFromMarginal(p, FALSE)
  expect_equal(dim(tt)[2], 7)
  expect_equal(dim(tt)[1], 5)
})
