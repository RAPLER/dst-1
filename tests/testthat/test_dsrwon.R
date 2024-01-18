# Tests "dsrwon" function
context("Compute unnormalized Dempster's Rule")
library(dst)
test_that("dsrwon", {
  # 
  # T1 x and y must be of class bcaspec. 
  # 
  x1 <- list(a=1:3, b="foo")
  y1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(dsrwon(x = x1, y = y1) , "One or more inputs not of class bcaspec.")
  #
  # T2 ncol of x and y must be equal. 
  # 
  x2 <- bca(tt = matrix(c(0,1,1,1,1,1,0,1,1,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c","d"),  varnames = "x2", idvar = 1)
  expect_error(dsrwon(x = x2, y = y1) , "Nb of elements of frame x and frame y not equal.")
  #
  # T3 Value names of the frames must identical and in the same order.
  # 
  z1 <- bca(tt = matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T"), varnames = "Diagnosis1", idvar = 1)
  z2 <- bca(tt = matrix(c(1,0, 0,1, 1,1), ncol=2, byrow=TRUE), m= c(0.01, 0.99, 0), cnames =c("C", "T"), varnames = "Diagnosis2", idvar = 2)
  expect_error(dsrwon(x = z1, y = z2) , "Value names of the two frames differ. Check value names of variables as well as their position.")
  #
  # T4 warning message when evidence is completely contradictory
  # 
  cnames <- c("yes","no")
  f1<- t(matrix(c(1,0,1,1),ncol=2))
  m1<- c(1,0)
  x1 <- bca(f1, m1, cnames)
  f2<- t(matrix(c(0,1,1,1),ncol=2))
  m2<- c(1,0)
  x2 <- bca(f2, m2, cnames)
  expect_warning(dsrwon(x = x1, y = x2) , 'Totally conflicting evidence \\(con = 1\\). Data is inconsistent.')
  #
  # T5 Check that the sum of masses is <= 1
  #
  zy1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  zx2 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"),  varnames = "x2", idvar = 1)
  zx2$spec[,2] <- c(0.3, 0.5, 0.3)
  expect_error(dsrwon(x = zy1, y = zx2) , "Invalid data, sum of masses of one vector, or both, greater than one.")
  #
  # T6 Check that nb of elements of frame in last of ssnames equal parameter infovar[,2]
  #
  zy1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  zx2 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"),  varnames = "x2", idvar = 1)
  zx2$infovar[,2] <- 2
  expect_error(dsrwon(x = zy1, y = zx2, use_ssnames = TRUE) , "Number of elements of frame differs from infovar parameter.")
  #
  # T7
  # Check if the  column names of the two tt matrices are in the same order
  X0 <- bca(tt = matrix(c(0,1,0,1, rep(1,4)), ncol = 4, byrow = TRUE), m = c(0.6,0.4), cnames = c("a", "b", "c", "d"), idvar = 1, varnames = "X0")
  X1 <- bca(tt = matrix(c(1,1,0,0, rep(1,4)), ncol = 4, byrow = TRUE), m = c(0.2,0.8), cnames = c("c", "a", "b", "d"), idvar = 1, varnames = "X1")
  #
  expect_error(dsrwon(x = X0, y = X1) , "Value names of the two frames differ. Check value names of variables as well as their position.")
})
