context("Calculation of an intersection")
library(dst)
test_that("inters_1", {
  # test fn with vectors
  mx <-c(1,1,0)
  names(mx) <-  c("a", "b", "c")
  my <-c(0,1,0)
  names(my) <-  c("a", "b", "c")
  result <- inters(mx,my)
  expect_error(result, regexp = NA) 
})
test_that("inters_2", {
  # test fn with matrices
  x3<-matrix(c(1,1,0,1), ncol=2, dimnames=list(NULL, c("a","b")))
  y3<-matrix(c(0,1,1,1), ncol=2, dimnames=list(NULL, c("a","b")))
  result <- inters(x3,y3)
  expect_error(result, regexp = NA) 
})
test_that("inters_3", {
  # test calculations
  mx<-matrix(c(0,1,0,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
  my<-matrix(c(0,0,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
  result <- inters(mx,my)
  expect_equal(dim(result),c(3,3,2))
  expect_equivalent(result[,,1], matrix(c(0,0,0,0,0,1,0,0,1), ncol = 3, byrow = TRUE))
  expect_equivalent(result[,,2], matrix(c(0,1,0,0,1,1,1,1,1), ncol = 3, byrow = TRUE))
})
  test_that("inters_error", {
    mx<-matrix(c(0,1,0,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
    rownames(mx) <- nameRows(mx)
    my<-matrix(c(0,0,1,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
    rownames(my) <- nameRows(my)
    expect_error(inters(mx,my), "Error in input arguments: check your input data.")
})