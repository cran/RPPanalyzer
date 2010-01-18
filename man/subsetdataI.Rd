\name{subsetdataI}
\alias{subsetdataI}
\docType{data}
\title{ Reverse phase protein array rawdata, samples serially diluted}
\description{
   The data Set is a list of four elements. Expression and background are matrices
   containing signal intensities, the data frames arraydescription and sampledescription
   comprising feature and phenodata.
   
}
\usage{data(subsetdataI)}
\format{
  list
}
\details{
The data set is a subset of \code{dataI}
}
\source{
 The data set contains original reverse phase protein array signals
  with randomized pheno and feature data.
}

\examples{
data(subsetdataI)
str(subsetdataI)
}
\keyword{datasets}
