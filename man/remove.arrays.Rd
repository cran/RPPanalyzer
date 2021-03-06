\name{remove.arrays}

\alias{remove.arrays}

\title{
Remove arrays from a RPPA data list
}

\description{
	Removes arrays from the RPPA data set which are not used in following calculations.
}

\usage{
remove.arrays(x, param = "target", arrays2rm = c("protein", "blank", "housekeeping"))
}

\arguments{
  \item{x}{
		List with RPPA data set
	}
  \item{param}{
		charater describing a row in the arraydescription (column in slidedescription file) 
	}
  \item{arrays2rm}{
		character defining the arrays to remove
	}
}

\value{
    The RPPA data list without the arrays specified by \code{arrays2rm}.
}

\author{
	Heiko Mannsperger <h.mannsperger@dkfz.de>
}

\examples{
\dontrun{
        library(RPPanalyzer)
        data(dataIII)
            
        DT <- remove.arrays(dataIII, param = "target", arrays2rm = c("protein"))
}
}

\keyword{ manip }
