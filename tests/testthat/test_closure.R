context("create closure matrix")
library(dst)
test_that("closure", {
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  ttxl <- lapply(1:nrow(ttx), function(i) ttx[i, ])
  ttyl <- closure(ttxl, FALSE)
  expect_equal(ttyl[[4]],c(0,1,0))
  
  ttx <- matrix(c(0,1,0,1,0,0,1,1,1), nrow=3, byrow = TRUE)
  ttxl <- lapply(1:nrow(ttx), function(i) ttx[i, ])
  ttyl <- closure(ttxl)
  expect_equal(ttyl[[4]],c(0,0,0))
  expect_equal(ttyl[[5]],c(1,1,0))
})
