# Tests "mFromMarginal" function
context("construct masses of a bca from marginal probabiltiies")
library(dst)
test_that("mFromMarginal", {
  p <- c(2,2,1.5,1.2,1,0,0)
  m <- mFromMarginal(p, FALSE)
  expect_equal(length(m), 5)
})
