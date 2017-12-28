test_that("productSpace", {
   # T1 check inputs: tt, spec, infovar
   # T1a check if tt is a matrix
  tt1 <- 1:3
  specnb1 <- c(1,1,2)
  infovar1 =matrix(c(5,7,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(productSpace(tt=tt1, specnb=specnb1, infovar = infovar1), "tt parameter must be a matrix.")
  # T1b check if the length of the specnb parameter is equal to the number of rows of tt
  tt1 <- matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
  specnb1 <- c(1,2)
  infovar1 =matrix(c(5,7,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(productSpace(tt=tt1, specnb=specnb1, infovar = infovar1), "specnb parameter must be a numeric vector of length nrow\\(tt\\)")
  # T1c check if the sum of the numbers of the 2nd column of infovar (frames dimensions) is equal to the number of columns of tt
  tt1 <- matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
  specnb1 <- c(1,1,2)
  infovar1 =matrix(c(5,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(productSpace(tt=tt1, specnb=specnb1, infovar = infovar1), "infovar parameter must be a 2 column matrix with sum of 2nd column = ncol\\(tt\\).")
  # T1d check if the spécification numbers are increased by one at most
  tt1 <- matrix(c(1,0,1,0,0,1,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames =list(NULL, c("foul", "fair", "foul", "fair")) )
  specnb1 <- c(1,1,3)
  infovar1 =matrix(c(5, 7, 2, 2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(productSpace(tt=tt1, specnb=specnb1, infovar = infovar1), "specnb values must be a sequence of numbers increasing by increments of 1 at most.")
})