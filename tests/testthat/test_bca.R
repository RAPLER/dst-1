# Tests "bca" function
context("define a bca")
library(dst)
test_that("bcaspec_1", {
  # T1: length(f[1,]) != length(cnames)
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m<- c(.9,.1)
  cnames <- c("yes","no")
  expect_error(bca(f, m, cnames = c("a", "b", "c") ) , "Error in input arguments: check your input data.")
  #
  # T2: length(f[,1])! = length(m)
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m = c(0.1, 0.2, 0.7)
  cnames <- c("yes","no")
  expect_error(bca(f,  m = c(0.1, 0.2, 0.7), cnames ) , "Error in input arguments: check your input data.")
  #
  # T3: abs(sum(m)-1) > 0.000001
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m = c(0.3, 0.8)
  cnames <- c("yes","no")
  expect_error(bca(f,  m = c(0.3, 0.8), cnames ) , "Error in input arguments: check your input data.")
  #
  # T4: Result is of class "bcaspec"
  # nb of value names == nb of columns of truth table matrix
  f<- t(matrix(c(1,0,1,1),ncol=2))
  m<- c(.9,.1)
  cnames <- c("yes","no")
  result <- bca(f, m)
  expect_is(result, "bcaspec")
  expect_equal(length(result$valuenames$v1), ncol(result$tt))  #  length of value names vector match with input table
  #
 # T5: More variable names than needed
 f<- t(matrix(c(1,0,1,1),ncol=2))
 m = c(0.3, 0.7)
 cnames <- c("yes","no")
 expect_error(bca(f, m, cnames, varnames = c("burglary", "boom") ), "number of variable names  not equal to number of variables" )
 #
  # T6: take names from "valuenames" parameter only if it is a string. If the matrix f is defined on a product space, the names must be the column names of the matrix. 
  tt1=matrix(c(rep(TRUE,3),FALSE, rep(TRUE,4)), ncol=4, byrow=TRUE)
  m=c(0.75, 0.25)
  infovar=matrix(c(4,5,2,2), ncol=2, dimnames =list(NULL, c("varnb", "size")))
  varnames = c("RdWorks", "Rain")
  valuenames <- list(Rain= c("Ry", "Rn"), RdWorks=c("rWdy", "rWdn") )
  result <- bca(tt = tt1, m, infovar = infovar, varnames = varnames, valuenames = valuenames)
  expect_equal(colnames(result$tt), c("col1", "col2", "col3", "col4"))
  #
  # T7: take names from "valuenames" parameter only if it is a string. If the matrix f is defined on a product space, the names must be the column names of the matrix. 
  tt1=matrix(c(rep(TRUE,3),FALSE, rep(TRUE,4)), ncol=4, byrow=TRUE)
  m=c(0.75, 0.25)
  cnames <- c("rWdy Ry", "rWdy Rn", "rWdn Ry", "rWdn Rn")
  infovar=matrix(c(4,5,2,2), ncol=2, dimnames =list(NULL, c("varnb", "size")))
  varnames = c("RdWorks", "Rain")
  valuenames <- list(Rain= c("Ry", "Rn"), RdWorks=c("rWdy", "rWdn") )
  result <- bca(tt = tt1, m, cnames, infovar = infovar, varnames = varnames, valuenames = valuenames)
  expect_equal(colnames(result$tt), c("rWdy Ry", "rWdy Rn", "rWdn Ry", "rWdn Rn"))
})