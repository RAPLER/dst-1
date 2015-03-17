\name{tabresults}
\alias{tabresul}
\alias{tabresulSingl}
\title{Prepare a table of results}
\usage{
  tabresul(x, remove = FALSE)
  tabresulSingl(x,  remove = FALSE)
}
\arguments{
  \item{x}{Input list in bca form.}
  \item{remove}{= TRUE removes focal elements with 0 mass.}
}
\description{
  The elements of the input list are:\cr
  DempsterRule: A matrix with masses in first column plus focal elements;\cr
  con: measure of conflicy between beliefs.
}
\value{
The input matrix is completed with calculations of belief, plausibility and the plausibility ratio, namely Pl(A)/Pl(not A).\cr
For tabresulSingl:  The output is restricted to the singletons.\cr
The result is a list of two elements:\cr
  1: the matrix of results (named mbp);\cr
  2: the measure of conflict (named con).
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
x1<-list(DempsterRule=matrix(c(.2,.5,.3,0,0,1,1,1,1,0,1,1),nrow=3),con=0)
  y1<-list(DempsterRule=matrix(c(.4,.6,0,1,0,1,1,1),nrow=2),con=0)
r<-nzdsr(dsrwon(x1,y1))
tabresul(r)
tabresulSingl(r)
## End(Not run)
}