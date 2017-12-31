# Tests "dsrwon" function
test_that("dsrwon", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(a=1:3, b="foo")
  y1 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", n=1)
  expect_error(dsrwon(x = x1, y = y1) , "One or more inputs not of class bcaspec.")
  # T2 ncol of x and y must be equal. 
  x2 <- bca(f=matrix(c(0,1,1,1,1,1,0,1,1,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c","d"),  infovarnames = "x2", n=1)
  expect_error(dsrwon(x = x2, y = y1) , "Nb of elements of frame x and frame y not equal.")
})