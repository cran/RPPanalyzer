`create.ID.col` <-
function(x,sample.id=c("sample_name","sample_treat")){
       identifier <- c(NULL)
               
      if (length(x)==4){  
   		for (i in sample.id){
   
   			identifier <- paste(identifier,as.character(x[[4]][,i]),sep="")
   			}
   			
       x[[4]] <- cbind(x[[4]],identifier)
      } else {
          for (i in sample.id){
   
   			identifier <- paste(identifier,as.character(x[[3]][,i]),sep="")
   			}
   			
       x[[3]] <- cbind(x[[3]],identifier)
      }  
   	 return(x)
}

