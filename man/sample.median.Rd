\name{sample.median}
\alias{sample.median}

\title{
Aggregate the replicates in an RPPA data set
}
\description{
	
   Aggregates the replicates in an RPPA data list with 3 or 4 elements using
   the median function.

}
\usage{
   sample.median(x, writetable = F)
}
\arguments{
  \item{x}{
      List with RPPA data set
}
  \item{writetable}{
		List containing RPPA data set
}
}

\value{
	\item{expression }{matrix with protein expression data}
	\item{error_mad }{matrix with error values}
	\item{arraydescription }{data frame with feature data}
	\item{sampledescription }{data frame with pheno data}
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz.de>
}

\examples{

	library(RPPanalyzer)
	
   data(dataI)
   dataI_bgcorr <- correctBG(dataI,method="normexp")
	
	data.median <- sample.median(dataI_bgcorr)

}
\keyword{ manip }

