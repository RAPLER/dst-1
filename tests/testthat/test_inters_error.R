test_that("inters_error", {
  mx<-matrix(c(0,1,0,0,1,1,1,1,1),nrow=3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c")))
  rownames(mx) <- nameRows(mx)
  my<-matrix(c(0,0,1,1,1,1,1,1),nrow=2, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c","d")))
  rownames(my) <- nameRows(my)
  expect_error(inters(mx,my), "Error in input arguments: check your input data.")
})