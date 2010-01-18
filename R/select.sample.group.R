`select.sample.group` <-
function(x,param="tissue",sel=c("T","N")){

   dat.lines <- c(NULL)
   
for (i in sel) {

   temp.lines <- which(x[[4]][,param]==i)
   dat.lines <- c(dat.lines,temp.lines)
   
  }
  
  x[[1]] <- x[[1]][dat.lines,]
  x[[2]] <- x[[2]][dat.lines,]
  x[[4]] <- x[[4]][dat.lines,]
  
  return(x)
  
}

