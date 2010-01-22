`read.gpr` <-
function(blocksperarray=4,spotter="arrayjet"){

## read in slidedescription as data.frame 
      slide.dat <- read.slidedescription()
## generate character vector with slidenames (gpr filenames) 
      slides <- slides.id(slide.dat)
## generate the array identifying character vector
      arrays <- array.id(slide.dat)
## create integer vector with numbers of arrays per slide
      arraysperslide <- arraysperslide(blocksperarray)
## calculate lines to skip for the first gpr file
    tbl <- read.table(slides[1],sep="\t",as.is=T,fill=T)
    lines2skip <- as.numeric(tbl[2,1])+3
## generate character vector as identifier for the single spots
    master.t <- read.table(slides[1],sep="\t",header=F,skip=lines2skip)
    linesperarray <- nrow(master.t)/arraysperslide[1]
    id <- master.t[c(1:linesperarray),5]
## define variables to store foreground and background values
    forg <- c(NULL)
    backg <- c(NULL)
## loop for filling foreground and background with data    
   for(i in seq(along=slides)){
   
   ## calculate lines to skip for the ith gpr file 
      	tbl <- read.table(slides[i],sep="\t",as.is=T,fill=T)
      	lines2skip<-as.numeric(tbl[2,1])+3
  	## read gpr data
      	data<-read.table(slides[i],skip=lines2skip)
  	## select numeric vector containing the generic block counts from gpr file
      	blocks <- data[,1]
  	## calculate the number of lines per array
      	linesperarray <- length(blocks)/arraysperslide[i]
  	## vector of length arraysperslide
      	count <- c(1:arraysperslide[i])
  	## generate integer vector with pad number for each line of the gpr file
      	pads <- rep(count,each=linesperarray)
  	## substitute block describing columns by a factorized pad describing column
      	data[,1] <- as.factor(pads)
  	## loop over the pads for creating a numeric background and foreground matrix 
   ## from a numeric vector
      	plevel <- levels(data[,1])
      	  for (j in seq(along=plevel)){
      	  padlines <- which(data[,1]==plevel[j])
      	  temp <- data[padlines,]
            forg <- cbind(forg,temp[,10])
            backg <- cbind(backg,temp[,14])
            }
      	}
    ## annotate rows and cols of the data matrixes  	
    rownames (forg) <- id
    colnames (forg) <- arrays
    rownames (backg) <- id
    colnames (backg) <- arrays
    ## store matrixes in list
    vals <- list(expression=forg, background=backg)
         ## substitute rownames if galfile produced by aushon software 
               if (spotter=="aushon"){
                  vals <- sub.ID(vals)
               }
    return(vals)
}

