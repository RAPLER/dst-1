\name{nzdsr}
\alias{nzdsr}
\title{
Normalization of Dempster's rule of combination, Shafer method.
}
\description{
A normalization of the result from the combination of two belief functions is done. If there is no non-zero mass allotted to the empty intersection, the result does not change. Otherwise, the normalization follows the method proposed by Shafer, which consist in dividing the results by 1 minus the mass allotted to the empty set.
}
\usage{
nzdsr(x)
}
\arguments{
  \item{x}{
The list x is the result of function dsrwon.
  }
}
\details{
 The input list x has four elements:\cr
  x$combination is the non-normalized result of the combination of the two tables by the Dempster's rule;\cr
  x$I12 is the intersection table;\cr
  x$TRI is a vector of indexes for the sort;\cr
  x$con is the measure of conflict between beliefs.
}
\value{
The result is a list y in bca form:\cr
  y$DempsterRule is the normalized result of the combination of the two tables by the Dempster's rule, following Shafer method of normalization;\cr
  y$con is the measure of conflict between beliefs.\cr
}
\references{
Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey. 296 p.
~ add pages here.
}
\author{
Claude Boivin, Stat.ASSQ.
}
\examples{
## Not run: 
x<-list(DempsterRule=t(matrix(c(.9,1,0,.1,1,1),ncol=2)),con=0)
y<-list(DempsterRule=t(matrix(c(.5,0,1,.5,1,1),ncol=2)),con=0)
nzdsr(dsrwon(x,y))
## End(Not run)
}