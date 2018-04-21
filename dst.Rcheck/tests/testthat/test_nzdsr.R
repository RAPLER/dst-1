# Tests "nzdsr" function
test_that("nzdsr", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", varnb = 1)
  expect_error(nzdsr(x1) , "Input argument not of class bcaspec.")
  # T2 ncol of x and y must be equal. 
  #
  # T3 Warning message when evidence is completely contradictory
  cnames <- c("yes","no")
  f1<- t(matrix(c(1,0,1,1),ncol=2))
  m1<- c(1,0)
  x1 <- bca(f1, m1, cnames, con = 1)
  expect_error(nzdsr(x = x1) , 'Completely conflicting evidence \\(con = 1\\). Data is inconsistent.')
})