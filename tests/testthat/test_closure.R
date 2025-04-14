context("create closure matrix")
library(dst)
test_that("closure", {
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  ttyl <- closure(ttx, FALSE)
  expect_equal(ttyl[4,],c(FALSE,TRUE,FALSE))
  
  ttx <- matrix(c(0,1,0,1,0,0,1,1,1), nrow=3, byrow = TRUE)
  ttyl <- closure(ttx)
  expect_equal(ttyl[4,],c(FALSE,FALSE,FALSE))
  expect_equal(ttyl[5,],c(TRUE,TRUE,FALSE))
  
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  ttyl <- closure(ttx, FALSE, TRUE)
  expect_equal(ttyl[4,],c(FALSE,TRUE,FALSE))
  
  ttx <- matrix(c(0,1,0,1,0,0,1,1,1), nrow=3, byrow = TRUE)
  ttyl <- closure(ttx)
  expect_equal(ttyl[4,],c(FALSE,FALSE,FALSE))
  expect_equal(ttyl[5,],c(TRUE,TRUE,FALSE))
})
