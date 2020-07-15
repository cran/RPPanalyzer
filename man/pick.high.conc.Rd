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
   pick.high.conc(x, highest = ("dilution"), sample.id=c("sample","sample.n"))
}

\arguments{
    \item{x}{Any RPPA data list with 4 elements}
    \item{highest}{Character string describing the column that contains the dilution steps}  
    \item{sample.id}{Attributes to identify the samples}
}

\details{
   The function selects all spots or samples from a RPPA data set with the value
   1 in the column of the sampledescription denoted in argument \code{highest}.
}

\value{
An RPPA data list containing only the samples with the highest concentration of each dilution series.
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz.de>, Stephan Gade <s.gade@dkfz.de>
}

\examples{
\dontrun{
   library(RPPanalyzer)
   data(ser.dil.samples)

   dataHighcon <- pick.high.conc(ser.dil.samples,highest="dilution")
}
}


\keyword{ manip }
