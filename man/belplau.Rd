% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/belplau.R
\name{belplau}
\alias{belplau}
\title{Calculation of the degrees of Belief and Plausibility of a basic chance assignment (bca).}
\usage{
belplau(x, remove = FALSE, h = NULL, method = NULL)
}
\arguments{
\item{x}{A basic chance assignment mass function (see \code{\link{bca}}).}

\item{remove}{= TRUE: Exclude subsets with zero mass.}

\item{h}{= NULL: Hypothesis to be tested. Description matrix in the same format than \code{x$tt}}

\item{method}{= NULL: Use Fast Zeta Transform ("fzt") or Efficient Zeta Transform ("ezt") or Efficient Zeta Transform on a meet-closed subset ("ezt-m")}
}
\value{
A matrix of \code{M} rows by 3 columns is returned, where \code{M} is the number of focal elements: \itemize{
 \item Column 1: the degree of Belief \code{bel};
 \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
 \item Column 3: the degree of Epistemic uncertainty \code{unc};
 \item Column 4: the degree of Plausibility \code{plau};
 \item Column 5: the Plausibility ratio \code{rplau}.
   }
}
\description{
Degrees of Belief \code{Bel} and Plausibility \code{Pl} of the focal elements of a bca are computed. The ratio of the plausibility of a focal element against the plausibility of its contrary is also computed. Subsets with zero mass can be excluded from the calculations.\cr
}
\details{
The degree of belief \code{Bel} is defined by: \cr
\deqn{bel(A) = Sum((m(B); B \subseteq A))}{bel(A) = Sum((m(B); B <= A))} for every subset B of A.\cr
The degree of plausibility \code{pl} is defined by: \cr
\deqn{pl(A) = Sum[(m(B); B \cap A \neq \emptyset]}{pl(A) = Sum[(m(B); (B & A) not empty]} for every subset \code{B} of the frame of discernment. \cr
The plausibility ratio of a focal element \code{A} versus its contrary \code{not A} is defined by:  \eqn{Pl(A)/(1-Bel(A))}.
}
\examples{
x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
byrow = TRUE), m = c(0.2,0.5, 0.3), 
cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
belplau(x)
y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
byrow = TRUE), m = c(0.6, 0.4),  
cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
xy <- nzdsr(dsrwon(x,y))
belplau(xy)
print("compare all elementary events")
xy1 <- addTobca(x = xy, tt = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = TRUE))
belplau(xy1) 
belplau(xy1, remove = TRUE) 
belplau(xy1, h = matrix(c(1,0,0,0,1,1), nrow = 2, byrow = TRUE))

}
\references{
\itemize{
\item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 39-43.
\item Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
}
}
\author{
Claude Boivin, Peiyuan Zhu
}
