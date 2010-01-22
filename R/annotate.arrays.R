#' Annotates columns of RPPA expression and background matrix
#'
#' The columns of RPPA expression and background matrix are annotated
#' using the rows "pad", "slide", "incubation_run" and "spotting_run" from the
#' array description
#' @param x list containing RPPA data
#' @param y data frame with array description meta data
#' @note for internal use only
#' @author Heiko Mannsperger <h.mannsperger@dkfz.de>
#' @return




`annotate.arrays` <-
function (x,y){

## x = list with two matixes from read.gpr() and if necessary followed by sub.ID()
## y = data frame returned from read.slidedescription

         array.id <- array.id(y)
            array.table <- t(y)
            array.table <- rbind(array.table,array.id)
            colnames(array.table) <- array.id
            checkarrays <- colnames(array.table)==colnames(x[[1]])
            if ( all(checkarrays)){
               x <- list(expression=x[[1]],background=x[[2]],arraydescription=array.table)
               return (x)
               }
            else{
            print("can not annotate expression matrix, please check slidedescription file")
            }
}

