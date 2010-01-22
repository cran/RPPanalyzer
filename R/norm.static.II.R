`norm.static.II` <-
function (x,normalizer=c("housekeeping"),writetable=F) {
         
         dat <- log(x[[1]])
         
         prot.cols <- which(x[[3]]["target",]==normalizer)
         
               ifelse ( length(prot.cols) > 1,
                     norm <- apply(dat[,prot.cols],1,mean),
                     norm <- dat[,prot.cols])
               
                temp <- apply(dat,2,function(x){
                              x-norm
                              })
                      data <- list(expression=temp,
                                    dummy=temp,
                                   arraydescription=x[[3]],
                                   sampledescription=x[[4]])
                                   
                      if(writetable){
                        write.Data(data)
                        }
                        
                      return(data)
}

