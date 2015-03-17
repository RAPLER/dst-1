\name{dsrwon}
\alias{dsrwon}
\title{dsrwon : Combination of two belief functions using Dempster's rule of combination, without normalization}
\usage{
  dsrwon(x, y)
}
\arguments{
  \item{x}{List of two elements in bca (basic chances assignment) form (bca on M focal elements and the measure of conflict between beliefs.}
  \item{y}{List of two elements in bca form (bca on N focal elements and the measure of conflict between beliefs.}
}
\description{
  Two belief functions are combined by the Dempster's rule. The first element of the lists contains the tables to combine, which are the matrix of focal elements plus the masses in column 1.
}
\details{
Each row of the tables identify a proposition (a subset of the frame of discernment) by a vector of (0,1) plus a mass in the (0,1] interval. A function (inters) does the intersection of the propositions. Another function (combmasses) does the product of the vectors of masses. Finally, a function (doubles) cleans the intersection of the duplicate elements.
}
\value{
The result is a list of four elements:\cr
  1: un-normalized Dempster's rule of combination;\cr
  2: a table of intersections;\cr
  3: indices for the sort of the propositions;\cr
  4: the measure of conflict between beliefs.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
 ## Not run:  
x<-list(DempsterRule=matrix(c(.2,.5,.3,0,0,1,1,1,1,0,1,1),nrow=3),con=0)
y<-list(DempsterRule=matrix(c(.4,.6,0,1,0,1,1,1),nrow=2),con=0)
dsrwon(x,y)
## End(Not run)
}