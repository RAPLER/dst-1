\name{plautrans}
\alias{plautrans}
\title{Plausibility transformation on the distribution of singletons}
\usage{
  plautrans(x)
}
\arguments{
  \item{x}{output list from function tabresul}
}
\seealso{
\code{\link{tabresul}}
}
\description{
The list x obtained from the tabresul function. We extract the singletons which appears in the input table as focal elements. We compute the plausibility transformation on the singletons.
}
\value{
The matrix of singletons with the plausibility transformation added in last column.
}
\author{
Claude Boivin, Stat.ASSQ
}
\references{
Cobb, B. R. and Shenoy, P.P. (2006). On the plausibility transformation method for translating belief function models to probability models. Journal of Approximate Reasoning, 41(3), April 2006, 314--330.
}
\examples{
## Not run: 
init<-initsing(3,c("a","b","c"))
x<-list(DempsterRule=matrix(c(.2,.5,.3,0,0,1,1,1,1,0,1,1),nrow=3),con=0)
y<-list(DempsterRule=matrix(c(.4,.6,0,1,0,1,1,1),nrow=2),con=0)
r1 <- nzdsr(dsrwon(init,x))
r<-nzdsr(dsrwon(r1,y))
plautrans(tabresul(r))
## End(Not run)
## ajouter comme test la présence du vecteur ign
## ajouter comme test qu'il faut le nb de singletons (tous) 
}
