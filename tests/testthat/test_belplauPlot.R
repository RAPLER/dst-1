# Tests "belplauPlot" function
context("plot a belplau object")
test_that("belplauPlot", {
  bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
  byrow = TRUE), m = c(0.2,0.5, 0.3), 
  cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  bel_plau <- belplau(bpa, h=diag(3))
  bel_plau_plot <- belplauPlot(bel_plau, c("1","2","3"), c(0,1,0), levels=c(0,1),is_log_scale=TRUE,is_negative = TRUE)
  bel_plau_plot <- belplauPlot(matrix(rep(1,101),ncol=1,dimnames=list(NULL,"rplau")), 1:101, c(rep(0,50),1,rep(0,50)), levels=c(1,0),is_log_scale=FALSE,is_negative = FALSE)
  bel_plau_plot <- belplauPlot(matrix(rep(1,101),ncol=1,dimnames=list(NULL,"rplau")), 1:101, c(rep("B",50),"A",rep("B",50)), levels=c("A","B"),is_log_scale=FALSE,is_negative = FALSE)
  expect_equal(bel_plau_plot$data$index[1], "1")
})
