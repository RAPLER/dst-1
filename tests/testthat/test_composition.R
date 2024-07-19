# Tests "composition" function
context("Calculate Schoder's composition of two relations")
library(dst)
test_that("composition",{
  # composing a three by three relation with itself
  R1 <- matrix(c(1,1,0,0,1,1,0,0,1), nrow=3, byrow = TRUE)
  expect_equal(composition(R1,R1), matrix(c(1,1,1,0,1,1,0,0,1), nrow=3, byrow = TRUE))
})
