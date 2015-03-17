\name{rplau}
\alias{rplau}
\title{Computation of the plausibility ratio}
\usage{
  rplau(x)
}
\arguments{
  \item{x}{A 2x2 matrix of belief (bel) and plausibility (pl) measures.}
}
\description{
We compute the plausibility ratios of a group of focal elements of the frame of discernment. The input matrix x is a result from the function belplau. We calculate the plausibility ratio of a proposition A as Pl(A)/(1-bel(A.))
}
\value{
For each focal element A, we obtain Pl(A)/Pl(not A)
}
\references{
Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run: 
x<-list(DempsterRule=matrix(c(.2,.5,.3,0,0,1,1,1,1,0,1,1),nrow=3),con=0)
y<-list(DempsterRule=matrix(c(.4,.6,0,1,0,1,1,1),nrow=2),con=0)
zr<-nzdsr(dsrwon(x,y))
zb<-belplau(zr$DempsterRule)
zrpl<-rplau(zb)
## End(Not run)
}

