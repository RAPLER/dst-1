# Tests "extmin" function
test_that("extmin", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(a=1:3, b="foo")
  y1 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", n=1)
  expect_error(extmin(x1, y1), "One or more inputs not of class bcaspec.")
  # T2 The two relations must have at least one variable in common.. 
  x2 <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), infovarnames= c("A"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  #
  y2 <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), infovarnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
  expect_error(extmin(x2, y2), "no common variable to the two relations.")
  #
  # T3 There must be at least one variable to add to the relation rel1
  y3 <- bca(f=matrix(c(1,0,1,0,1,1), ncol=2), m=c(0.3,0.5,0.2), cnames=c("true", "false"), infovar=matrix(c(4,2), ncol = 2, dimnames = list(NULL, c("varnb", "size"))), infovarnames= c("B"), inforel= matrix(c(7,1), ncol = 2, dimnames = list(NULL, c("relnb", "depth"))))
})