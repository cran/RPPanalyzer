`arraysperslide` <-
function(blocksperarray=4){

## read in slidedescription table
      slide.dat <- read.slidedescription()
## generate vector with gprfile names from slidedescription table
      slides <- slides.id(slide.dat)
      
      arrays.slide <- c(1:length(slides))
      for (i in seq(along=slides)){
        ## read in gpr file
            tbl <- read.table(slides[i],sep="\t",as.is=T,fill=T)
        ## calculate number in the gpr file which describes how many lines to skip    
            lines2skip <- as.numeric(tbl[2,1])+3
        ## read in values from gpr file (without header)
            master.t <- read.table(slides[i],sep="\t",header=F,skip=lines2skip)
        ## claculate number of arrays per slide
            val <- max(master.t [,1])/blocksperarray
        ## store number of arrays in vector
            arrays.slide[i] <- val
      }
      
## return vector with integers describing the number of arrays per slide
      return (arrays.slide)
}

