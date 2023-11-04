# Tests "belplauLogsumexp" function
context("Compute belief and plausibility measures with logsumexp")
library(dst)
test_that("belplauLogsumexp", {
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
  byrow = TRUE), m = c(0.2,0.5, 0.3), 
  cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  belplau(x)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
  byrow = TRUE), m = c(0.6, 0.4),  
  cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  xy <- nzdsr(dsrwon(x,y))
  expect_equal(belplauLogsumexp(xy),belplau(xy))
  
  xy1 <- addTobca(x = xy, tt = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = TRUE))
  H <- matrix(c(1,0,0,0,1,1), nrow = 2, byrow = TRUE)
  expect_equal(belplauLogsumexp(x,h=H),belplau(x,h=H))
})
