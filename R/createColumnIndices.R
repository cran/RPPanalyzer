#'
#' createColumnIndices.R
#'


#'
#' grep the column names of the header to find the right columns
#'
createColumnIndices <- function(col.names) {

    
    colIndices <- vector("list", length=7)
    names(colIndices) <- c("F", "B", "Block", "Row", "Column", "ID", "Flags")

    colIndices$F <- grep("^F.*Mean$", col.names, perl=T)
    colIndices$B <- grep("^B.*Mean$", col.names, perl=T)
    colIndices$Block <- grep("Block", col.names, perl=T)
    colIndices$Row <- grep("Row", col.names, perl=T)
    colIndices$Column <- grep("Column", col.names, perl=T)
    colIndices$ID <- grep("ID", col.names, perl=T)
    colIndices$Flags <- grep("Flags", col.names, perl=T)

    return(colIndices)

}
