\name{ser.dil.samples}
\alias{ser.dil.samples}
\docType{data}
\title{ Reverse phase protein array rawdata, samples serially diluted}
\description{
   The data Set is a list of four elements. Expression and background are matrices
   containing signal intensities, the data frames arraydescription and sampledescription
   comprising feature and phenodata.
   
}
\usage{data(ser.dil.samples)}
\format{
  list
}
\details{
The data set is a subset of the data set dataI to shorten the running time during
the R CMD check process. The data set contains information about the localization of
the samples.
}
\source{
 The data set contains original reverse phase protein array signals
  with randomized pheno and feature data.
}

\examples{
\dontrun{
data(ser.dil.samples)
str(ser.dil.samples)
}
}
\keyword{datasets}
