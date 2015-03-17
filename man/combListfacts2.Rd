\name{combListfacts}
\alias{dstCombListfacts2}
\alias{dstCombListfacts}
\alias{probCombListfacts}
\title{
Combine a serie of facts with Dempster's rule
}
\description{
A loop on the rows of the input table is done.\cr
For dstCombListfacts: Each row is completed by allotting the complementary mass the the frame.\cr
For dstCombListfacts2: Each set of rows of the same number is completed by allotting the complementary mass the the frame.\cr
For probCombListfacts: works only on  a "yes,no" frame. Each row is completed by the complementary mass and subset. Dempster's rule is applied until there is no row left.
}
\usage{
dstCombListfacts2(data_table)
dstCombListfacts(table)
probCombListfacts(table)
}
\arguments{
  \item{table}{
A matrix of facts.\cr
Each row of the matrix contains a mass (number within the (0,1] interval) and the description of a focal element.\cr
Example with three facts for function probCombListfacts \cr
    Mass  yes	no	 \cr
  	  0.1	  1	  0	 \cr 
  	  0.1	  0	  1	 \cr 
 	    0.3	  0	  1	 \cr 
 }
 \item{data_table}{
Same matrix of facts plus a column of numbers which indicate the elements belonging to the same fact .\cr
Example with three facts for function dstCombListfacts2 \cr
     Mass    a    b     c    No    \cr
      0.4    0    1    0    1 \cr
      0.2    1    0    0    1 \cr
      0.3    1    0    1    3 \cr
      0.1    0    0    1    3 \cr
      0.6    1    0    0    5 \cr
}
}
\value{
The result y is a list of two elements in the bca form:\cr
 DempsterRule: this is the normalized result of the combination of the two tables by the Dempster's rule, following Shafer method of normalization.\cr
  con: this is the mass allotted to the empty set.
}
\author{
Claude Boivin, Stat.ASSQ.
}
\examples{
## Not run: 
combfacts<-matrix(c(0.1,1,0,0.1,0,1,0.3,0,1),nrow=3,byrow=TRUE)
probCombListfacts(combfacts)
combfacts2<-matrix(c(0.4,0,1,0,1,0.2,1,0,0,1,0.3,1,0,1,3,0.1,0,0,1,3,0.6,1,0,0,5),nrow=5,byrow=TRUE)
dstCombListfacts2(combfacts2)
## End(Not run)
}