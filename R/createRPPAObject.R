#'
#' createRPPAObject
#'
#' creates an RPPA object used in SuperCurve
#' @param x: the RPPA data list
#' @param arrayIdx: the index of the array, the RPPA object is created
#'
createRPPAObject <- function(x, arrayIdx, block.design) {

    # the bg corrected expression values of the samples
    # of the specified array
    Mean.Net <- x[[1]][,arrayIdx]

    # the names of the samples
    Sample <- x[[4]][,"identifier"]

    # now we calculate the main row and the main col
    # we use this terms to determine the rows and cols of the clocks
    # note: the blocks are ordered rowwise, e.g 16 blocks
    #  1  2  3  4
    #  5  6  7  8
    #  9 10 11 12
    # 13 14 15 16

    # first the Main.Row, it can be expressed as
    # (block-1) div (number of cols in the blockdesign) + 1
    Main.Row <- ((x[[4]][,"Block"]-1) %/% block.design[2]) + 1

    # second the columns which can be expressed as
    # (block-1) mod (number of cols in the blockdesign) + 1
    Main.Col <- ((x[[4]][,"Block"]-1) %% block.design[2]) + 1
    
    # now we can copy our Col and Row information as Sub.Col and Sub.Row
    Sub.Row <- x[[4]][,"Row"]
    Sub.Col <- x[[4]][,"Column"]


    # finally create data.frame
    data <- data.frame(Main.Row=Main.Row, Main.Col=Main.Col, Sub.Row=Sub.Row, Sub.Col=Sub.Col, Sample=Sample, Mean.Net=Mean.Net)


    # now define the file and the antibody atribute of the RPPA object
    file <- x[[3]]["gpr",arrayIdx]
    antibody <- x[[3]]["target",arrayIdx]

    # return the new created class RPPA
    return(new("RPPA", data=data, file=file, antibody=antibody))
    
}

