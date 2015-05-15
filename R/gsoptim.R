gsoptim<-function(g,int,e=1e-6){
  
  # Check parameters
  if(length(int)!=2) stop("The length of the interval must be 2")
  if(e<=0) stop("Tolerance needs to be positive")
  if(!is.function(g)) stop("g needs to be a function")
  if(length(formals(g))!=1) stop("g needs to be a function that takes only one element as an argument")
  
  
  alpha<-(3-sqrt(5))/2
  it<-0
  x1<-int[1]
  x3<-int[2]	
  while(abs(x3-x1)>e){
	  x0<-x1+alpha*(x3-x1)	
	  x2<-x3-alpha*(x3-x1)
	  if(g(x0)<g(x2)) x3<-x2 
	  else  x1<-x0 
	  it<-it+1	
	}
  min=(x1+x3)/2
  return(new("gss",min = min,  int.lower=int[1],int.upper=int[2], it=it, g=g))
}	

