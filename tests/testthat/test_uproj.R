# Tests "uproj" function
context("Product space extension")
library(dst)
test_that("uproj", {
  # T1 x and y must be of class DSMspec. 
  x1 <- list(a=1:3, b="foo")
  y1 <- DSM(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(uproj(x1, y1), "One or more inputs not of class DSMspec.")
  #
  # T2 The two relations must have at least one variable in common.. 
  x2 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("A"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  #
  y2 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  expect_error(uproj(x2, y2), "No common variable to the two relations. Check variable names and numbers.")
#   #
#   # T3 Test removed
#   # There must be at least one variable to add to the relation rel1
#   y3 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
# #
#   expect_error(uproj(y3, y3), "No missing variable. Check your inputs.")
#
# T4 relRef must have names of variables in "varnames" parameter
#
x4 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
#
ss_D <- SSR(varnames= "D", idvar = 4, size = 2,cnames=c("true", "false") )
ss_E <- SSR(varnames= "E", idvar = 5, size = 2,cnames=c("true1", "false1") )
ss_DE <- list(ss_D, ss_E) 
spec_DE <-  matrix(c(1,2,3,0.3,0.5,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
y4 <- jointDSM(tt = matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4, dimnames = list(NULL, unlist(sapply(ss_DE, `[[`,4)))), spec = spec_DE, ssrlist = ss_DE )
#
expect_error(uproj(x4, y4), "Variables names of rel not in relRef. Check variables names.")
#
# T5, Check that names of rel1 are in relRef
x5 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))

y5 <- jointDSM(tt=matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4, dimnames = list(NULL, c("true", "false","true", "false"))), spec =   matrix(c(1,2,3,0.3,0.5,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))), infovar=matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames = c("C", "D"), relnb=7)
#
expect_error(uproj(x5, y5), "Variables names of rel not in relRef. Check variables names.")
#
# T6, Check that variable names and numbers are equal
x6 <- DSM(tt = matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))

y6 <- jointDSM(tt=matrix(c(1,0,1,0,1,1,1,0,1,0,1,1), ncol=4, dimnames = list(NULL, c("true", "false","true", "false"))), spec =   matrix(c(1,2,3,0.3,0.5,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))), infovar=matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), varnames = c("B", "C"), relnb=7)
#
expect_error(uproj(x6, y6), "Variables names and variables numbers do not match. Check variables names, numbers and their position.")
})