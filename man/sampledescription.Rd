\name{sampledescription}
\alias{sampledescription}
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
   To fit a model to serial dilution e.g. using the \code{calcSdc} function, it
   is neccessary to indicate the highest concentration in the \code{dilution} column
   with the value 1.
   Any additionally column can be added to describe further phenodata of interest.
   
}
\source{

 The data set contains original reverse phase protein array signals
  with randomized pheno and feature data.
}

\keyword{datasets}
