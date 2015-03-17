\name{smppmexample}
\alias{smppmexample}
\title{Smets' [2003] Example: Peter, Paul or Mary}
\usage{
  smppmexample()
}
\description{
  This is an example of the use of functions of the package to combine two belief functions with Dempster's rule.
}
\author{
Claude Boivin, Stat.ASSQ
}
\examples{
## The function is currently defined as
function() {
  sm1<-matrix(c(1,1,0,0,0,1,1,1,1),nrow=3,byrow=TRUE)
  v1<-c(0.5,0.5,0)
  sm2<-matrix(c(0,1,1,1,1,1),nrow=2,byrow=TRUE)
  v2<-c(1,0)
  names_s1s2<-c("Peter","Paul","Mary")
  smppm1l<-bca(v1,sm1,names_s1s2)
  smppm2l<-bca(v2,sm2,names_s1s2)
  rppml<-initsing(3,names_s1s2)
  rppml<-nzdsr(dsrwon(rppml,smppm1l))
  rppml<-dsrwon(rppml,smppm2l)
  print("Result from function dsrwon: Dempster's rule before normalization")
  print(rppml)
  rppml<-nzdsr(rppml)
  print("Result frum function nzdsr: normalization")
  print(rppml)
  print( "Function belplau")
  print(belplau(rppml$DempsterRule))
  print("Function tabresul")
  print(tabresul(rppml))
}
}
