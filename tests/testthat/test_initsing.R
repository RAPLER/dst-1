test_that("initsing", {
  # check result
  result <- initsing(3,cnames = c("a","b","c"))
  expect_equivalent(result$combination, matrix(c(0,1,0,0,0,0,1,0,0,0,0,rep(1,5)), ncol = 4, byrow = TRUE))
  # check naming the variable
  result <- initsing(3,cnames = c("a","b","c"), infovarnames = "choices")
  expect_equal(names(result$infovaluenames[1]), "choices")
})