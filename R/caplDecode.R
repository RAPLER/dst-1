caplDecode<-function(ind,base) { 
	if (length(base) == 1)
		base<-array(base,aplShape(ind)) 
	# b<- base^c((aplShape(ind)-1):0)
	base<-base[aplShape(base):1]	
     b<-c(1,cumprod(butLast(base)))
 	b<-b[aplShape(b):1]	
	return(sum(b*ind))
}