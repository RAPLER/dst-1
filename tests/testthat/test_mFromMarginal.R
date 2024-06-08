# Tests "mFromMarginal" function
context("construct masses of a bca from marginal probabiltiies")
library(dst)
test_that("mFromMarginal", {
  p <- c(2,2,1.5,1.2,1,0,0)
  m1 <- mFromMarginal(p, simple = TRUE)
  expect_equal(length(m1), 2)
  m2 <- mFromMarginal(p, simple = FALSE)
  expect_equal(length(m2), 5)
})
