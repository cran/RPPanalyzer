\name{dataII}
\alias{dataII}
\docType{data}
\title{ Reverse phase protein array data, samples from a stimulation time course}
\description{
  The data Set is a list of four elements. \code{Sample.median} and \code{sample.mads} are matrices
   containing logged signal intensities and errors, the data frames arraydescription and sampledescription
   comprising feature and phenodata.
}
\usage{data(dataII)}
\format{
   List
}
\details{
  The data set is a list of four elements with data of a original reverse phase
array experiment. The elements \code{Sample.median} and \code{sample.mads} are 624 times 12 matrices
containing logged signal intensities and errors for every sample of the
experiment. The values are background corrected and normalized against total protein content.
Arraydescription is a data frame, describing the incubation of every array
refering the column of the matrices. Required rows are target
and AB_ID with characters and array.id (four integers linked with "-").
Sampledescription is a data frame according to the rows of the
matrices annotating the samples. The columns "sample", "stimulation",
"inhibition", "stim_concentration", and "time"
  are describing the time course experiment.
}
\source{
  The data set contains original reverse phase protein array signals
  from a stimulation time course experiment with randomized pheno and feature data.
}

\examples{
data(dataII)
str(dataII)
}
\keyword{datasets}
