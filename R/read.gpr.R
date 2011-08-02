`read.gpr` <-
function(blocksperarray=4,spotter="arrayjet",...){


    ## get additional arguments to read.delim
    readArgs <- list(...)

    ## read in slidedescription as data.frame 
    slide.dat <- read.slidedescription()

    ## generate character vector with slidenames (gpr filenames) 
    slides <- slides.id(slide.dat)
    
    ## generate the array identifying character vector
    arrays <- array.id(slide.dat)
    
    
    ## is a foreground and background is specified?
    ## read.slidedescription checks if both or none is specified, so we can check for foreground only
    sigSpec <- "foreground" %in% colnames(slide.dat)

        
    ## calculate lines to skip for the first gpr file
    ## at first get the seperator for the files if given, default is tab
    if(!is.null(readArgs$sep)) {
        seperator <- readArgs$sep
    }
    else {
        seperator <- "\t"
    }

    ## get the second line of the header 
    tbl <- readLines(slides[1], n=2)[2]
    lines2skip <- as.numeric(strsplit(tbl, split=seperator)[[1]][1])+2
    ## generate character vector as identifier for the single spots
    master.t <- read.delim(slides[1], header=T,skip=lines2skip, check.names=F,...)

    ## get the column defining the foreground and background signal columnns in the gpr file
    if(sigSpec) {
        fColumn <- unique(slide.dat[slide.dat$gpr==slides[1],"foreground"])[1]
        bColumn <- unique(slide.dat[slide.dat$gpr==slides[1],"background"])[1]
    }
    else {
        fColumn <- NULL
        bColumn <- NULL
    }

    ## get the indices of the required columns
    indices <- createColumnIndices(colnames(master.t), foreground=fColumn, background=bColumn) 

    ## get only these column of the master table
    master.t <- master.t[,unlist(indices)]
    ## set right colnames
    colnames(master.t) <- names(indices)

    ## order master table to block row column of the array localization
    o <- order(master.t[,"Block"], master.t[,"Row"], master.t[,"Column"])
    master.t <- master.t[o,]

    ## compute the arrays per slide
    arraysPerSlide <- max(master.t[,"Block"])/blocksperarray

    linesperarray <- nrow(master.t)/arraysPerSlide
    id <- master.t[c(1:linesperarray),"ID"]

    ## get the localization of the spots and save them
    localization <- master.t[c(1:linesperarray),c("Block", "Column", "Row")]
    colnames(localization) <- c("Block", "Column", "Row")
    
    ## define variables to store foreground and background values
    forg <- NULL
    backg <- NULL
    flags <- NULL


    ## loop for filling foreground and background with data    
    for(i in seq(along=slides)){

        ## calculate lines to skip for the ith gpr file 
        tbl <- readLines(slides[i], n=2)[2]
        lines2skip <- as.numeric(strsplit(tbl, split=seperator)[[1]][1])+2

        ## read gpr data
        data<-read.delim(slides[i],skip=lines2skip, check.names=F,...)

        ## extract colnames of the foreground and background signal from the slidedescription
        if(sigSpec) {
            fColumn <- unique(slide.dat[slide.dat$gpr==slides[i],"foreground"])[1]
            bColumn <- unique(slide.dat[slide.dat$gpr==slides[i],"background"])[1]
        }
        else {
            fColumn <- NULL
            bColumn <- NULL
        }

        ## get the indices of the required columns
        indices <- createColumnIndices(colnames(data), foreground=fColumn, background=bColumn) 

        ## get only these column of the dataable
        data <- data[,unlist(indices)]
        ## set right colnames
        colnames(data) <- names(indices)

        ## order data table to block row column of the array localization
        o <- order(data[,"Block"], data[,"Row"], data[,"Column"])
        data <- data[o,]

        ## select numeric vector containing the generic block counts from gpr file
        blocks <- data[,"Block"]

        ## claculate number of arrays per slide
        arraysPerSlide <- max(blocks)/blocksperarray

        ## calculate the number of lines per array
        linesperarray <- length(blocks)/arraysPerSlide
        ## vector of length arraysperslide
        count <- c(1:arraysPerSlide)
        ## generate integer vector with pad number for each line of the gpr file
        pads <- rep(count,each=linesperarray)
        ## substitute block describing columns by a factorized pad describing column
        data[,"Block"] <- as.factor(pads)
        ## loop over the pads for creating a numeric background and foreground matrix 
        ## from a numeric vector
        plevel <- levels(data[,"Block"])
        for (j in seq(along=plevel)){
            padlines <- which(data[,"Block"]==plevel[j])
            temp <- data[padlines,]
            forg <- cbind(forg,temp[,"F"])
            backg <- cbind(backg,temp[,"B"])
            flags <- cbind(flags,temp[,"Flags"])
        }
    }
    ## annotate rows and cols of the data matrixes  	
    rownames (forg) <- id
    colnames (forg) <- arrays
    rownames (backg) <- id
    colnames (backg) <- arrays
    rownames (flags) <- id
    colnames (flags) <- arrays

    ## store matrixes in list
    vals <- list(expression=forg, background=backg,Flags=flags, localization=localization)
    ## substitute rownames if galfile produced by aushon software 
    if (spotter=="aushon"){
        vals <- sub.ID(vals)
    }
    return(vals)
}

