\name{initsing}
\alias{initsing}
\title{
Initialization of a matrix of singletons
}
\description{
The purpose is to allow the calculation of the plausibility transformation later. Each singleton has a zero mass. The matrix is completed by the whole set which receive a mass of one.
}
\usage{
initsing(card,names)
}
\arguments{
  \item{card}{The number of singletons.}
  \item{names}{A character vector of names of the elements of the frame of discernment.}
}
\value{
A list in bca form., i.e. a matrix of n+1 rows for the n singletons and the whole set. The measure of conflict is set at 0.
}
\author{
Claude Boivin, Stat.ASSQ.
}
\examples{
## Not run:
initsing(3,c("Peter","Paul","Mary"))
## End(Not run)
}