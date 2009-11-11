`norm.BCA` <-
function(x,proteinc="BCA",writetable=F){
         
         dat <- log(x[[1]])
         
         cf <- log(x[[4]][,proteinc])
         
         texas <- apply(dat,2,function(x){
                                    x-cf
                                    })
         data <- list(  expression=texas,
                        dummy=texas,
                        arraydescription=x[[3]],
                        sampledescription=x[[4]])
                        
          if(writetable){
            write.Data(data)
            }
            
         return(data)
}

