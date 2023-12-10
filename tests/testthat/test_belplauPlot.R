# Tests "belplauPlot" function
context("plot a belplau object")
test_that("belplauPlot", {
  bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
  byrow = TRUE), m = c(0.2,0.5, 0.3), 
  cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  bel_plau <- belplau(bpa, h=diag(3))
  bel_plau_plot <- belplauPlot(bel_plau, c("1","2","3"), c(0,1,0),is_log_scale=TRUE)
  expect_equal(bel_plau_plot$data$index[1], "1")
})
