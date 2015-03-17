\name{inters}
\alias{inters}
\title{Intersection of two groups of propositions}
\usage{
  inters(x, y)
}
\arguments{
  \item{x}{A matrix of M focal elements defined on a frame of K elements.}
   \item{y}{A matrix of N focal elements defined on a frame of K elements.}
}
\description{
The intersection of of the M propositions of the first matrix is done with the N propositions of the second matrix.
}
\value{
A table of dimension M x K x N.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
x<-matrix(c(.2,.5,.3,0,0,1,1,1,1,0,1,1),nrow=3)
y<-matrix(c(.4,.6,0,1,0,1,1,1),nrow=2)
z<-inters(x,y)
## End(Not run)
}

