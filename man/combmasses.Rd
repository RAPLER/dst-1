\name{combmasses}
\alias{combmasses}
\title{Product of two vectors of masses}
\usage{
  combmasses(x, y)
}
\arguments{
  \item{x}{A matrix of propositions, i.e. 1 col of masses plus boolean matrix describing focal elements.}
  \item{y}{A matrix of propositions, i.e. 1 col of masses plus boolean matrix describing focal elements.}
}
\description{
The two vectors of masses (first column of each matrix) are multiplied.  
}
\details{
The sum of the elements of each vector of masses must add to 1.
}
\value{
The result of the product is a matrix of masses with dimensions nrow(x) by nrow(y).
}
\author{
Claude Boivin, Stat.ASSQ
}


