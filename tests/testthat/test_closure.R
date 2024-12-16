context("create closure matrix")
library(dst)
test_that("closure", {
  sourceCpp("R/closure.cpp")
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  ttxl <- lapply(1:nrow(ttx), function(i) ttx[i, ])
  ttyl <- ttxl
  ttyl <- closure(ttxl,ttyl)
})
