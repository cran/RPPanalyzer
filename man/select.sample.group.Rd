\name{select.sample.group}
\alias{select.sample.group}
\title{
	Selects samples from RPPA data
}
\description{
	Selects samples from an RPPA data list according to the selected parameter.
}
\usage{

	select.sample.group(x, params=list("tissue" = c("T", "N")), combine = F )

}
\arguments{
  \item{x}{List with RPPA data set}
  \item{params}{List of parameters the selection of samples is bases on. The names of the list describes the columns of the sampledescription matrix. The according values corresponds to the values in these columns that will be selected.
}
   \item{combine}{Logical value. Indicates wheter the samples should match at least one criterion given in the params list (\code{combine=TRUE}) or if all criteria should be met (\code{combine=FALSE}). Default is \code{FALSE}.}
}

\value{
An RPPA data list containing only these samples that match the criteria given in the \code{params} list.
}

\author{
	Heiko Mannsperger <h.mannsperger@dkfz.de>, Stephan Gade <s.gade@dkfz.de>
}

\examples{
\dontrun{
	library(RPPanalyzer)
	data(dataII)

	selectedData <- select.sample.group(dataII,params=list("stimulation"=c("A","B")))
}
}
\keyword{ manip }
