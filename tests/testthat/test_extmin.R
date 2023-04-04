# Tests "extmin" function
context("Product space extension")
library(dst)
test_that("extmin", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(a=1:3, b="foo")
  y1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(extmin(x1, y1), "One or more inputs not of class bcaspec.")
  #
  # T2 The two relations must have at least one variable in common.. 
  x2 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("A"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  #
  y2 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  expect_error(extmin(x2, y2), "No common variable to the two relations. Check variable names and numbers.")
#   #
#   # T3 Test removed
#   # There must be at least one variable to add to the relation rel1
#   y3 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
# #
#   expect_error(extmin(y3, y3), "No missing variable. Check your inputs.")
#
# T4 relRef must have names of variables in "varnames" parameter
#
x4 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
#
y4 <- bca(tt = matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4), m=c(0.3,0.5,0.2), cnames=c("true", "false","true", "false"), infovar=matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= NULL, inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
#
expect_error(extmin(x4, y4), "Variables names of rel not in relRef. Check variables names.")
#
# T5, Check that names of rel1 are in relRef
x5 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))

y5 <- bcaRel(tt=matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4, dimnames = list(NULL, c("true", "false","true", "false"))), spec =   matrix(c(1,2,3,0.3,0.5,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))), infovar=matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames = c("C", "D"), relnb=7)
#
expect_error(extmin(x5, y5), "Variables names of rel not in relRef. Check variables names.")
#
# T6, Check that variable names and numbers are equal
x6 <- bca(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))

y6 <- bcaRel(tt=matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4, dimnames = list(NULL, c("true", "false","true", "false"))), spec =   matrix(c(1,2,3,0.3,0.5,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))), infovar=matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames = c("B", "C"), relnb=7)
#
expect_error(extmin(x6, y6), "Variables names and variabless numbers do not match. Check variables names, numbers and their position.")
})