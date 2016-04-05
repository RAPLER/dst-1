\name{bca}
\alias{bca}
\title{State a basic chance assignment over subsets of a frame of discernment
}
\usage{
  bca(m,f,n, no)
}
\arguments{
  \item{m}{A vector of masses associated to the focal elements.}
   \item{f}{A matrix constructed in a boolean style (0,1) or a boolean matrix. The number of columns of the matrix is the number of elements of the frame of discernment.}
  \item{n}{A character vector of names of the elements of the frame of discernment.}
  \item{no}{A number. Used to create product spaces of bca"s. 0 if NULL}
}
\description{
Use this function to express a mass function over subsets of a finite set of elements, called the frame of discernement.
}
\details{
Each line of the matrix describe a focal element, i.e. a subset of the frame. Each column identify an element of the frame of discernment. The length of the vector m must match the number of rows of the matrix f. The sum of the elements of m must add to 1. The length of the vector n must match the number of columns of the matrix f.
}
\value{
A list of two elements:
  \item{DempsterRule}{ The matrix f with the masses in column 1}
  \item{con}{The measure of conflict, set at 0}
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
f<- t(matrix(c(1,0,1,1),ncol=2))
m<- c(.9,.1)
n<- c("yes","no")
bca(m,f,n)
## End(Not run)
}
