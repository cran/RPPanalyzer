#'
#' calcSuperCurve.R
#'
#' Uses the package SuperCurve to perform the quantification
#'


#'
#' calcSuperCurve
#' uses the package SuperCurve for quantification
#' 
calcSuperCurve <- function(x,model="cobs", method="nlrq", sample.id=c("sample","sample.n"),sel=c("measurement","control"), dilution="dilution", block.design, plot=T) {

    if(!require(SuperCurve)) {
    
        stop("This function requires the package SuperCurve. Please install it from 'http://bioinformatics.mdanderson.org/Software/OOMPA/'. Otherwise use the function 'calcSdc' instead.")

    }

    # at first use create.ID.col to create unique identifiers   
    xi <- create.ID.col(x,sample.id=sample.id)

    # get only the real measurements, don't fit blank spots and buffer
    xi <- select.sample.group(xi,params=list("sample_type"=sel))

    # get the unique IDs
    id <- unique(as.character(xi[[4]]$identifier))

    # create a matrix of the fittet relative expression values
    # since we do a fit for every array the no. of columns is equal to the no. of columns of the raw expression matrix
    # the no. of rows is equal to the length of unique(id)
    vals <- matrix(NA, ncol=ncol(xi[[1]]), nrow=length(id))
    colnames(vals) <- colnames(xi[[1]])
    rownames(vals) <- id

    # iterate over all arrays and get an individual fit
    # this is the main loop
    for(i in 1:ncol(xi[[1]])) {

        cat("Fit SuperCurve model for array:", xi[[3]]["target",i], "\n")
        cat("SuperCurve model:", model, "\n")
        cat("SuperCurve fit method:", method, "\n")

        # create an RPPA object
        rppa <- createRPPAObject(xi, i, block.design) 

        # create an RPPA Design Object
        rppaDesign <- createRPPDesignObject(rppa, xi, dilution)

        # do the fit
        fit <- RPPAFit(rppa=rppa, design=rppaDesign, measure="Mean.Net", model=model, method=method) 
        #fit <- try(do.call("RPPAFit", args=list(rppa=rppa, design=rppaDesign, measure="Mean.net", model=model, method=method)))

        #if(inherits(fit,"try-error"))  {

#stop("This function requires the package SuperCurve. Please install it from 'http://bioinformatics.mdanderson.org/Software/OOMPA/'.")

        #}


        # plot the fit if wanted
        if(plot) {

            main=c(paste("Target:",xi[[3]]["target",i]), paste("Antibody:", xi[[3]]["AB_ID",i]), paste("Model:", model))

            plot(fit, main=main)

        }

        # order the results according to our sampledescription
        conc <- as.numeric(fit@concentrations)
        m <- match(rownames(vals), names(fit@concentrations), nomatch=0)
        if(any(m==0)) {
            stop("For some samples there was no model fit. Something went wrong!")
        }

        conc <- conc[m]
    
        vals[,i] <- conc

        cat("Finished!\n\n")
 
    }


    # get only these entries from each dilution series with the highest concentration
    # we will need the sampledescription of this
    tempDat <- pick.high.conc(xi)

    # pick the lines of the sample description matching to our fitted values
    m <- match(rownames(vals), tempDat[[4]]$identifier)
    # discarding columns like concentration, dilution, Block, etc.
    # these information don't make sense any longer
    n <- colnames(tempDat[[4]]) %in% c("concentration", "dilution", "Block", "Column", "Row")
    sampleDesc <- tempDat[[4]][m,!n]

    # set rownames of vals to the sample IDs
    rownames(vals) <- sampleDesc$ID

    # assemble new RPPA data list for return
    ret <- tempDat
    ret[[1]] <- vals
    ret[[2]] <- vals
    ret[[4]] <- sampleDesc

    # set new names
    namesRet <- names(ret)
    namesRet[1] <- "expression"
    namesRet[2] <- "dummy"
    names(ret) <- namesRet

    # return the result
    return(ret)

}
