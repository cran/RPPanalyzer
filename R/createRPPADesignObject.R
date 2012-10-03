#'
#' createRPPDesignObject
#'
#' creates an RPPA Design object based on an RPPA object and the RPPA data list
#' @param rppa: an RPPA object
#' @param x: an RPPA data list which is the source of the RPPA object rppa
#'
createRPPDesignObject <- function(rppa, x, dilution="dilution") {

    # create the column Samples needed for the RPPA Design objects
    # each dilution series is denoted as one replicate
    #tt <- by(x$sampledescription, 
             #list(   x$sampledescription[,dilution],
                     #x$sampledescription$identifier), 
             #function(xx) {
                
                 #return(cbind(xx, data.frame(Series=paste(xx$identifier, paste("Rep", 1:nrow(xx), sep=""),sep="."))))
                
            #})

    ## combine the data frames in the list
    #ttt <- do.call("rbind", tt)

    ## reorder the result to have the same ordering like the original sample description
    #o <- order(x$sampledescription$identifier,x$sampledescription[,dilution])
    #ttt[o,] <- ttt


    # create a new RPP Design object
    #return(RPPADesign(rppa, steps=log2(ttt[,dilution]), series=ttt$Series))

    return(RPPADesign(rppa, steps=log2(x[[4]][,dilution]), x[[4]]$identifier))
}
