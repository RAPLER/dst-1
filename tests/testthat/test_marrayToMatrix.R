# Tests "marrayToMatrix" function
test_that("marrayToMatrix", {
  # T1 Nb of vars of x and infovar must match. 
  x <- array(c(0,1,0,0,0,0,0,1,0,1,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,1), c(2,2,7), dimnames = list( RdWorks=c("rWdy", "rWdn") , Rain=c("Ry", "Rn"), ev=1:7))
  infovar <- matrix(c(4,5,6,2,2,3), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(marrayToMatrix(x, infovar), "Number of varables of the two parameters not compatible." )
  # T2 Dimension of x and infovar must match.
  x <- array(c(0,1,0,0,0,0,0,1,0,1,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,1), c(2,2,7), dimnames = list( RdWorks=c("rWdy", "rWdn") , Rain=c("Ry", "Rn"), ev=1:7))
  infovar <- matrix(c(4,5,2,3), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(marrayToMatrix(x, infovar), "Dimensions not compatible." )
} )
