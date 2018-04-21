test_that("shape", {
# test vector lengtx
z1 <- c("a", "b", "c")
result <- shape(z1)
expect_equal(result, 3)
# test matrix dim
z2 <- matrix(c(1:6), nrow=3)
result <- shape(z2)
expect_equal(result, c(3,2))
})