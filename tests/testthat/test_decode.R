context("Convert data in another number system")
Library(dst)
test_that("decode", {
  # test from base to decimal
  result <- decode(c(2,2,2,2), c(1,0,1,1))
  expect_equal(result, 11)
  # left argument extended
  result <- decode(2, c(1,0,1,1))
  expect_equal(result, 11)
  # right argument extended
  result <- decode(c(365,24,60), c(1,57)) 
  expect_equal(result, 117)
  # transform 2 days 1 h 57 min in minutes
  result <- decode(c(365,24,60), c(2,1,57))
  expect_equal(result, 2997)
  # polynomial 1*x^2 +2*x +3 evaluated at x=1.5
  result <- decode(1.5, c(1,2,3))
  expect_equal(result, 8.25)
})