context("create commSparse")
library(dst)
test_that("commSparse", {
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  tty <- closure(ttx, FALSE)
  commSparse()
})