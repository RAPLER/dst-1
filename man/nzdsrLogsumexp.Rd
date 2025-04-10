% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nzdsrLogsumexp.R
\name{nzdsrLogsumexp}
\alias{nzdsrLogsumexp}
\title{Normalization of a basic chance assignment with logsumexp}
\usage{
nzdsrLogsumexp(x, sparse = "no")
}
\arguments{
\item{x}{A basic chance assignment, i.e. a object of class bcaspec.}

\item{sparse}{Put "yes" to use sparse matrix. Default = "no".}
}
\value{
z The normalized basic chance assignment.
}
\description{
It may occur that the result of the combination of two basic chance assignments with Dempster's Rule of combination contains a non-zero mass allocated to the empty set. The function \code{nzdsr} normalizes the result of function \code{dsrwon} by dividing the mass value of the non-empty subsets by 1 minus the mass of the empty set.
}
\examples{
x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
m = c(0.9,0.1), cnames = c("yes", "no"),
varnames = "x", idvar = 1)
x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE), 
m = c(0.5,0.5), cnames = c("yes", "no"), 
varnames = "x", idvar = 1)
print("combination of x1 and x2")
x1x2 <- dsrwon(x1,x2, varname = "x")
nzdsr(x1x2) 
# Test with sparse matrices
x1s=x1
x2s=x2
x1s$tt <- methods::as(x1$tt, "RsparseMatrix")
x2s$tt <- methods::as(x2$tt, "RsparseMatrix")
x1x2s <- dsrwon(x1s, x2s, use_ssnames = TRUE)
nzdsr(x1x2s)

print("normalization of a bca definition.")
y2 <- bca(tt = matrix(c(0,0,0,1,0,0,1,1,1),nrow = 3, 
byrow = TRUE), m = c(0.2,0.5,0.3), 
cnames = c("a", "b", "c"), idvar = 1)
cat("y2")
cat("\  ")
y2
nzdsr(y2)  
}
\references{
Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
}
\author{
Claude Boivin, Peiyuan Zhu
}
