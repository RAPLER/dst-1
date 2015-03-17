\name{dempster}
\alias{dempster}
\title{
Calculation of Dempster's rule with normalization
}
\description{
Two tables x and y are combined by the Dempster's rule. Each line of the tables identify a proposition, i.e. a subset of the set of possibilities by a vector of (0,1) plus a mass in the (0,1] interval. The function inters make the intersection of propositions. The function combmasses does the product of the vectors of masses. Another function (doubles) clean the intersection of duplicate elements.
}
\usage{
dempster(x,y)
}
\arguments{
  \item{x}{
table of M lines par K hypothesis + 1 column of masses
}
  \item{y}{
table of N lines par K hypothesis + 1 column of masses
}
}
\value{
The result of dempster is a matrix where the first column contains the mass allotted to the empty set. Each line of the rest of the matrix contains the normalized mass for the set described by a vector of (0,1). Note that this function gives the same numerical result than the functions nzdsr(dsrwon(x,y) where x and y are basic chance assignments formatted as lists.
}
\references{
Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey. 296 p.
- Dempster's rule : pp 57-60.
}
\author{
Claude Boivin, Stat.ASSQ.
}
\examples{
## Not run: 
# Combining two pieces of Evidence
x<-t(matrix(c(.9,1,0,.1,1,1),ncol=2))
y<-t(matrix(c(.5,0,1,.5,1,1),ncol=2))
dempster(x,y)
## End(Not run)
}
