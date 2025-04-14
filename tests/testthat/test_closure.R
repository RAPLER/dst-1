context("create closure matrix")
library(dst)
test_that("closure", {
  ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
  tty <- closure(ttx, FALSE)
  expect_equal(tty[4,],c(FALSE,TRUE,FALSE))
  
  ttx <- matrix(c(0,1,0,1,0,0,1,1,1), nrow=3, byrow = TRUE)
  tty <- closure(ttx)
  expect_equal(tty[4,],c(FALSE,FALSE,FALSE))
  expect_equal(tty[5,],c(TRUE,TRUE,FALSE))
  
  ttx <- methods::as(matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE), "RsparseMatrix")
  tty <- closureSparse(ttx, FALSE, TRUE)
  expect_equal(tty[4,],c(0,1,0))
  
  ttx <- methods::as(matrix(c(0,1,0,1,0,0,1,1,1), nrow=3, byrow = TRUE), "RsparseMatrix")
  tty <- closureSparse(ttx, display_progress=TRUE)
  expect_equal(tty[4,],c(0,0,0))
  expect_equal(tty[5,],c(1,1,0))
})
