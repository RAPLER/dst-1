test_that("productSpace", {
  # T1: input must be a list
  x <- c(1,1,3)
  expect_error(productSpace(x), "Input is not a list.")
  # T2 names of the list must be: tt, spec, infovar
  x <- list(a=1, b=2, c=3)
  expect_error(productSpace(x), "List member infovar missing.")
})