context("Construct a description matrix from a list of subsets names.")
library(dst)
test_that("ttmatrix", {
  # T1. input not a list
  x1 <- c("a","b")
  expect_error(ttmatrix(x1), "Input must be a list.")
  # T2. test boolean matrix
  subsets_names <- list(c("b", "c"), "b", c("a", "b", "c"))
  tt1<-ttmatrix(subsets_names)
  expect_equal(as.logical(tt1[1,]),c(FALSE, TRUE, TRUE))
  # T3. test boolean matrix with emptyset
  tt2<-ttmatrix(c("Empty",subsets_names))
  expect_equal(as.logical(tt2[1,]),c(FALSE, FALSE, FALSE))
  # T4. test sparse matrix
  tt3<-ttmatrix(subsets_names, "yes", c("a","b","c"))
  expect_equal(as.logical(tt3[1,]),c(FALSE, TRUE, TRUE))
  # T5. test sparse matrix with emptyset
  tt4<-ttmatrix(c("Empty",subsets_names), "yes", c("a","b","c"))
  expect_equal(as.logical(tt4[1,]),c(FALSE, FALSE, FALSE))
})
