\name{doubles}
\alias{doubles}
\title{Remove duplicates lines of a matrix}
\usage{
  doubles(x)
}
\arguments{
  \item{x}{A matrix of M focal elements defined on a frame of K elements.}
}
\description{
  This recursive function removes the duplicate rows of a table.
}
\value{
The resulting table is the input table minus the duplicate rows.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## Not run:
# Example 1 
td1<-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1),ncol=3,byrow=TRUE)
zzz<-doubles(td1)
# Example 2
td2<-matrix(c(1:3, 1:3,4:6,1:3),nrow=4,byrow=TRUE)
zzz<-doubles(td2)
# Example 3
td3<-matrix(c("d","e","f", rep(c("a","b","cc"),times=3),"g","h","i"),nrow=5,byrow=TRUE)
zzz<-doubles(td3)
# Example 4
td4<-matrix(as.logical(td1),nrow=5,byrow=TRUE)
zzz<-doubles(td4)
## End(Not run)
}

