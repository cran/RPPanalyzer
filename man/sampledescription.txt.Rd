\name{sampledescription.txt}
\alias{sampledescription.txt}
\docType{data}
\title{ sample description file}
\description{

    The sample description file contains all information concerning the samples
    of a reverse phase protein experiment.

}

\format{

  tab delimeted text file
}
\details{

   The sample description file contains information for sample annotation and data analysis.
   To identify the sample in the source well plate the columns \code{plate}, \code{row}, \code{column}
   are obligatory. It is neccessary that every well that is spottet is described.
   The columns \code{sample_type} and \code{sample} as well as \code{concentration} and
   for serially diluted samples \code{dilution} are required for data analysis.
   The column \code{dilSeriesID} is required for background correction based on serial dilutions.
   Any additionally column can be added to describe further phenodata of interest.
   
}
\source{

 The data set contains original reverse phase protein array signals.
 A549 cells were starved for 24 h and subsequently stimulated with six different HGF concentrations 
 ranging from 0 - 100 ng/ml. 
 Samples were obtained at six different time points ranging from 0 - 120 min. 
 The experiment was done in triplicates, and the samples were analysed by RPPA using antibodies 
 directed against proteins and phosphoproteins of MET receptor signalling.
}

\keyword{datasets}
