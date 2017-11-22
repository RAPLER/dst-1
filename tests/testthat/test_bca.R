# Tests "bca" function
f<- t(matrix(c(1,0,1,1),ncol=2))
m<- c(.9,.1)
cnames <- c("yes","no")
result <- bca(f, m)
expect_is(result, "bcaspec")