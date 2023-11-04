# Tests "logsum" function
context("adding small probabilities")
library(dst)
test_that("logsum", {
  expect_equal(exp(logsum(log(1e-5),log(1e-5))),1e-5+1e-5)
})
