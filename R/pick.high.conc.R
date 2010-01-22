`pick.high.conc` <-
function(x,highest=("dilution")){
     lines <- which(x[[4]][,highest]==1)
     
           x[[1]] <- x[[1]][lines,]
           x[[2]] <- x[[2]][lines,]
           x[[4]] <- x[[4]][lines,]
           
     return(x)
}

