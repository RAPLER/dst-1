# Tests "PT (plautrans)" function
context("Plausibility transformation")
library(dst)
test_that("PT", {
  # T1 x and y must be of class DSMspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "x1", idvar = 1)
  expect_error(PT(x1) , "Input argument not of class DSMspec.")
  # T2 check that the input is a normalized DSM
  x2 <- DSM(tt = matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), varnames = "x2", idvar = 1)
  expect_error(PT(x2) ,"Invalid data: Empty set among the focal elements. Normalization necessary. See function normalize.")
})