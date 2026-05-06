# Tests "printDSM" function
context("print (0,1) matrix and mass vector ofa DSM")
library(dst)
test_that("printDSM_1", {
  # T1 x must be of class DSMspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", varnb = 1)
  expect_error(normalize(x1) , "Input argument not of class DSMspec.")
})