\name{belplau}
\alias{belplau}
\title{Calculation of the measures of belief and plausibility for each focal element of a matrix}
\usage{
  belplau(x, remove = FALSE)
}
\arguments{
  \item{x}{A matrix of propositions, i.e. 1 col of masses plus boolean matrix describing focal elements.}
  \item{remove}{= TRUE removes focal elements with 0 mass.}
}
\description{
  The input table x is a matrix of focal elements with their mass, generally obtained as the result of the function nzdsr (Dempster's rule of combination). Elements with zero mass can be removed if present. Measures of belief and plausibility are obtained.
}
\details{
The belief function bel is defined by:\cr
  bel(A)=Sum(m(B); B <= A), for every subset A of the frame of discernment.\cr
The plausibility function pl is defined by:\cr
 pl(A)=Sum(m(B); B & A not empty), for every subset A of the frame of discernment.
}
\value{
A matrix of m rows by 2 columns, where m is the number of rows of the input matrix.
}
\author{
Claude Boivin, Stat.ASSQ
}
\references{
Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey. 296 p.
}
\examples{
## Not run: 
x<-list(DempsterRule=t(matrix(c(.9,1,0,.1,1,1),ncol=2)),con=0)
y<-list(DempsterRule=t(matrix(c(.5,0,1,.5,1,1),ncol=2)),con=0)
r1<-dsrwon(x,y)
r<-nzdsr(r1)
bel<-belplau(r$DempsterRule)
## End(Not run)
}

