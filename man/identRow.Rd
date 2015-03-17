\name{identRow}
\alias{identRow}
\title{Naming the rows of a table of results}
\usage{
  identRow(x)
}
\arguments{
  \item{x}{The matrix describing M focal elements on the N elements of a frame of discernment.}
}
\description{
  The columns names of the frame of discernment are used to name each row of the matrix according to the position of the values "1" (or TRUE) in the description of the focal element.
}
\value{
The resul is a character vector of length M, i.e. the number of rows of the input matrix.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
f1=t(matrix(c(1,0,0,0,0,0,1,0,1,1,1,1),ncol=3))
mf1=c(.6,.4,0)
n<-c("A","B","C","D")
w1<-bca(mf1,f1,n)
r<-nzdsr(dsrwon(w1,w1))
tab<-tabresul(r)
identRow(tab$mbp[,1:length(n)])
## End(Not run)
}