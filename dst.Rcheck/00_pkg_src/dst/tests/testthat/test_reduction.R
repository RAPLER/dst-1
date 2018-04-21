test_that("reduction", {
  # T1 Apply reduction to a numeric vector
  result <- reduction(c(1,2,3,4), f="-")
  expect_equal(result, -8)
  # T2 Apply reduction to a logical vector
  result <-  reduction(c(1,0,1,1,0), f="&")
  expect_equal(result, FALSE)
  #T3 Apply reduction to a string vector
  result <- reduction(c("a", "b", "c"), f="paste")
  expect_equal(result, "a b c")
})