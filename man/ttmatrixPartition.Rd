% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttmatrixPartition.R
\name{ttmatrixPartition}
\alias{ttmatrixPartition}
\title{Create partition matrix}
\usage{
ttmatrixPartition(n, m)
}
\arguments{
\item{n}{partition size}

\item{m}{size of the set to be partitioned}
}
\value{
h binary partition matrix of size n by m
}
\description{
Create partition matrix
}
\examples{
# test singleton hypotheses
x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
pa <- ttmatrixPartition(x$infovar[2], x$infovar[2])
belplau(x, h=pa)
}
\author{
Peiyuan Zhu
}
