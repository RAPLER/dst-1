# Tests "nzdsr" function
test_that("nzdsr", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", varnb = 1)
  expect_error(nzdsr(x1) , "Input argument not of class bcaspec.")
  # T2 ncol of x and y must be equal. 
})