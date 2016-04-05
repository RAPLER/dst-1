\name{dotprod}
\alias{dotprod}
\title{Generalized inner product of two matrix}
\usage{
  dotprod(SR, BD, g, f)
}
\arguments{
  \item{SR}{A matrix of M rows by K columns.}
  \item{BD}{A matrix of K rows by N columns.}
  \item{g}{An operator. It can be any of : +, -, *, /, &, |, ==, <=, paste etc.}
  \item{f}{A second operator.}
}
\description{
The generalized inner product of two matrix combines two operators in the same way than a matrix product. The matrix have to be chosen in accordance with the desired result.
}
\details{
The number of rows of the second matrix must equal the number of columns of the first.
}
\value{
The result is a matrix of dimension M by N.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
# matrix product
x<-y<-matrix(c(1:6),nrow=2,byrow=TRUE)
dotprod(x,t(y),g="+",f="*")
# [,1] [,2]
# [1,]   14   32
# [2,]   32   77
# Find some data in a large matrix
x2<-matrix(c(1,0,0,1,1,1),nrow=2,byrow=TRUE)
y2<-matrix(c(1,0,0,0,1,0,1,1,0,0,1,1,1,1,1),nrow=5,byrow=TRUE)
dotprod(x2,t(y2),g="&",f="==")
#       [,1]  [,2]  [,3]  [,4]  [,5]
# [1,]  TRUE FALSE FALSE FALSE FALSE
# [2,] FALSE FALSE FALSE FALSE  TRUE
# Find the indices for the calculation of a belief function
x3<-matrix(c(0,0,1,1,1,0),nrow=2,byrow=TRUE)
y3<-matrix(c(1,0,0,0,1,0,0,0,1,1,1,0,1,1,1),nrow=5,byrow=TRUE)
dotprod(x3,t(y3),g="&",f="<=")
#      [,1]  [,2]  [,3]  [,4] [,5]
# [1,] FALSE FALSE  TRUE FALSE TRUE
# [2,] FALSE FALSE FALSE  TRUE TRUE
# Find the indices for the calculation of a plausibility function
dotprod(x3,t(y3),g="|",f="&")
# [,1]  [,2]  [,3]  [,4] [,5]
# [1,] FALSE FALSE  TRUE FALSE TRUE
# [2,]  TRUE  TRUE FALSE  TRUE TRUE
# Combine strings
str1 <- matrix(c("foul", "fair"), ncol = 2)
str2 <- matrix(c("true", "false"), ncol = 2)
dotprod(t(str1), str2,g="paste",f="paste")
## End(Not run)
}

