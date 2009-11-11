\name{select.sample.group}
\alias{select.sample.group}
\title{
	Selects samples from RPPA data
}
\description{
	Selects samples from an RPPA data list according to the selected parameter.
}
\usage{
	select.sample.group(x, param = "tissue", sel = c("T", "N"))
}
\arguments{
  \item{x}{
		List with RPPA data set
}
  \item{param}{
		character describing the parameter from which the samples should be selected
}
  \item{sel}{
  		character describing the parameter that should be selected
}
}

\value{
	\item{expression }{matrix with protein expression data}
	\item{expression_b }{matrix with protein expression data}
	\item{arraydescription }{data frame with feature data}
	\item{sampledescription }{data frame with pheno data}
}

\author{
	Heiko Mannsperger <h.mannsperger@dkfz.de
}

\examples{
	library(RPPanalyzer)
	
	data(dataII)
	selectedData <- select.sample.group(dataII,param="stimulation",sel=c("A","B"))
}
\keyword{ manip }
