`read.slidedescription` <-
function (){

    ## check for slidedescription.txt in working directory
    allfiles <- dir()
    if ( ! "slidedescription.txt" %in% allfiles) {
         stop("can not find slidedescription.txt in current working directory.")
    }
    
    ## read in data frame
    incubation<-read.delim("slidedescription.txt",header=T)
    
    ## check header for required columns
    reqCols <- c("gpr","pad","slide","incubation_run","spotting_run","target")
      if ( !all( reqCols %in% colnames(incubation))){
          stop("slidedescription file: columns are missing or header incorrect!")
      }
    ## check source plate description
      if ( mode(incubation[,"pad"])!="numeric" 
         | mode(incubation[,"slide"])!="numeric" 
         | mode(incubation[,"incubation_run"])!="numeric"
         | mode(incubation[,"spotting_run"])!="numeric"){
          stop ("slidedescription file: data format in columns pad, slide, incubation_run and spotting_run have to be numeric!") 
      }
    return(incubation)
}

