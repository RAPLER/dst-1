bca<-function(m,f,n, no = NULL){
# State a basic chance assignment over subsets of a frame of discernement
  if((abs(sum(m)-1)>0.000001) | (length(f[,1])!=length(m)) | (length(f[1,])!=length(n))){ 
    stop("error in input arguments: check your input data ") 
    }
  else{
    z<-cbind(m,f)
    colnames(z)<-c("mass",n)
    if (missing(no)) 
      { no <- 0 }
    y<-list(DempsterRule=z,con=0, no = no)
    return(y)
  }
 }