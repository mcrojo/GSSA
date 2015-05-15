
setMethod(f = "[", signature = "gss", definition=function(x, i, drop="missing"){
            
	if(i == "min"){return(x@min)}else{}
	if(i == "int.lower"){return(x@int.lower)}else{}
	if(i == "int.upper"){return(x@int.upper)}else{}
	if(i == "it"){return(x@it)}else{}
	if(i == "g"){return(x@g)}else{}
    
})


setMethod(f = "[[", signature = "gss", definition=function(x, i, drop="missing"){

	if(i == "min"){return(x@min)}else{}
	if(i == "int.lower"){return(x@int.lower)}else{}
	if(i == "int.upper"){return(x@int.upper)}else{}
	if(i == "it"){return(x@it)}else{}
	if(i == "g"){return(x@g)}else{}
                     
})


## PRINT
setMethod("show", "gss",function(object){

	cat("The minimum is:\n")
	print(object@min)
       
})


## PLOT
setMethod(f="plot", signature = "gss",  definition = function(x, y, ...){
  
	g<-as.function(x@g)
	sequence<-seq(x@int.lower,x@int.upper,length.out=1000)
	plot(sequence,g(sequence), main="Minimum by Golden Section Search", ylab=expression(g(x)), xlab=expression(x), type="l",...)
	points(x@min,g(x@min), lwd=2, col="red", pch=18)

})
          
          




setReplaceMethod(f = "[", signature = "gss", definition=function(x,i,value){

	if(i == "min"){return(x@min)}else{}
	if(i == "int.lower"){return(x@int.lower)}else{}
	if(i == "int.upper"){return(x@int.upper)}else{}
	if(i == "it"){return(x@it)}else{}
	if(i == "g"){return(x@g)}else{}
	return(x)

})


setReplaceMethod(f = "[[", signature = "gss", definition=function(x,i,value){
  

	if(i == "min"){return(x@min)}else{}
	if(i == "int.lower"){return(x@int.lower)}else{}
	if(i == "int.upper"){return(x@int.upper)}else{}
	if(i == "it"){return(x@it)}else{}
	if(i == "g"){return(x@g)}else{}
	return(x)



})


## SUMMARY
setMethod(f="summary", signature = "gss",  definition = function(object){

	x<-object
	TAB <- matrix(c(x@min,x@int.lower,x@int.upper,x@it),ncol=1)
	colnames(TAB)="Value"
	rownames(TAB)<-c("Minimum of the function:","Initial interval (lower):","Initial interval (upper):","Iterations until convergency:") 
	print(TAB)
  
})
