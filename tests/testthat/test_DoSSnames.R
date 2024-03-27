context("Construct list of subsets names from a description matrix.")
library(dst)
test_that("DoSSnames", {
  # T1. No column namest
  x1 <- matrix(c(0,1,1,1,1,1,1,0,1), ncol = 3)
  expect_error(DoSSnames(x1), "Column names missing. Add column names to your mamtrix.")
})
