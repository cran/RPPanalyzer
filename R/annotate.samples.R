`annotate.samples` <-
function (x,y){
    line<-match(rownames(x[[1]]),y[,1])
    sample.data<-y[line,]
    cols <- colnames(y)
    x <- list(expression=x[[1]],background=x[[2]],
    arraydescription=x[[3]],sampledescription=sample.data)
    colnames(x[[4]]) <- cols
    print(head(sample.data))
    return(x)
}

