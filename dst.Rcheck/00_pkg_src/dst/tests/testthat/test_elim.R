# Tests "elim" function
test_that("elim", {
  # T1 rel must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", varnb = 1)
  expect_error(elim(x1, 4) , "Rel input not of class bcaspec.")
  #
  # T2 rel must be composed of at least two variables
  f<- t(matrix(c(1,0,1,1),ncol=2))
  #' m<- c(.9,.1)
  #' cnames <- c("yes","no")
  #' x2 <- bca(f, m, cnames)
  #'  expect_error(elim(x2, 1) , "Input is not a relation. No variable to eliminate.")
  # T3 The number xnb must be in the list of numbers of the relation (variable varnb of the infovar table). 
  ttrwf= matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),nrow=4, byrow = TRUE, dimnames =list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
  specrw = matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
  inforw =matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  x2 <- bcaRel(tt = ttrwf, spec = specrw, infovar = inforw, infovarnames = c("RdWorks", "Rain"), relnb = 6)
  expect_error(elim(x2, 1), "Invalid variable number.")
})