\name{read.Data}
\alias{read.Data}
\title{
   Read and Annotate RPPA rawdata
}
\description{
	reads sampledescription and slidedescription txt files
   and annotates the median expression value in GenePix 	result files stored
   in current working directory.
}
\usage{
   read.Data(blocksperarray = 4, spotter = "arrayjet", writetable = FALSE)
}
\arguments{
  \item{blocksperarray}{
      Integer describing the number of blocks in one array.
      }
  \item{spotter}{
      character strings: default \code{arrayjet} or \code{aushon}.
   }
  \item{writetable}{ logical. If true data are exported as tab delimited text
      files to current working directory
  }
}
\details{
   This function reads and annotates RPPA rawdata provided in three different
   kind of files.
   It is very important that these data files are in a correct format and stored
   in the same folder.

   The file sampledescription.txt has to be a tab delimited
   text file with at least 6 columns named plate, column, row, sample_type, sample,
    concentration and in case of serially diluted samples a column dilution is required.
   The first 3 columns are describing the location of the sample in the source well plate.
   The 4th column describes the for different types of samples: measurement, control,
   neg_control or blank. In the column sample any character string describing the sample is possible.
   The column concentration has to contain only numerical values. Columns
  with further phenodata can be added.
  
  The slidedescription.txt describes the array properties. Required columns are:
  gpr (describing the name of the corresponding gpr file), the columns
    pad, slide, incubation_run, spotting_run containing integers are generating a unique array identifier.
    The column target describes the analyzed target and AB_ID the used antibody.
    Column with further feature data can be added.
    
    The third kind of files are the gpr files as results from image analysis
    software GenePix using the galfile from a aushon or arrayjet spotter.
   
}
\value{
   \item{expression }{matrix with protein expression data}
   \item{background}{matrix with background data}
   \item{arraydescription }{data frame with feature data}
   \item{sampledescription }{data frame with pheno data}
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz-heidelberg.de>
}

\examples{
  library(RPPanalyzer)

  dataDir <- system.file("data",package="RPPanalyzer")
  setwd(dataDir)

  exData <- read.Data()
}
\keyword{ manip }

