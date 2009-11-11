\name{pick.high.conc}
\alias{pick.high.conc}
\title{
   Select the highest concentration from serialy diluted samples
}
\description{
	Picks the dilution step with the value 1 from serialy diluted samples in an
   RPPA data set.
}
\usage{
   pick.high.conc(x, highest = ("dilution"))
}
\arguments{
  \item{x}{
      Any RPPA data list with 4 elements
}
  \item{highest}{
      character string describing the column that contains the dilution steps
}
}
\details{
   The function selects all spots or samples from a RPPA data set with the value
   1 in the column of the sampledescription denoted in argument \code{highest}.
}
\value{
\item{expression }{matrix with protein expression data}
\item{expression_b }{matrix with protein expression data}
\item{arraydescription }{data frame with feature data}
\item{sampledescription }{data frame with pheno data}
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz.de>
}

\examples{
   library(RPPanalyzer)
   data(subsetdataI)

   dataHighcon <- pick.high.conc(subsetdataI,highest="dilution")

}
\keyword{ manip }
