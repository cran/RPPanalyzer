\name{select.measurements}
\alias{select.measurements}

\title{
   Selects the measurement samples from an RPPA data list
}
\description{
   Selects the measurement samples defined as "measurement" in sample_type from
   an RPPA data list
}
\usage{
   select.measurements(x)
}
\arguments{
  \item{x}{
		List with RPPA data set
}
}

\value{
	\item{expression }{matrix with protein expression data}
	\item{background }{matrix with protein background data or error values
   dependend on the input files}
	\item{arraydescription }{data frame with feature data}
	\item{sampledescription }{data frame with pheno data}
}

\author{
	Heiko Mannsperger <h.mannsperger@dkfz.de>
}

\examples{
	library(RPPanalyzer)
	data(dataIII)
	dataIII_median <- sample.median(dataIII)
	measures <- select.measurements(dataIII_median)
	}
\keyword{ manip }
