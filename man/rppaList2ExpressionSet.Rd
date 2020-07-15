\name{rppaList2ExpressionSet}
\alias{rppaList2ExpressionSet}

\title{
   Convert RPPA data into Expression Set
}
\description{
   Converts a RPPA data list into an Expression Set
}
\usage{
   rppaList2ExpressionSet(x)
}
\arguments{
  \item{x}{
		List with RPPA data set}
}
\details{
    This function builds an Expression Set from RPPA data. Due to the design of
    RPPA experiments, pheno and feature data are inverted compared to DNA/RNA array
    data sets.
}
\value{
	object of class Expressionset
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz.de>
}
\examples{
\dontrun{
	library(RPPanalyzer)
	data(dataI)
   dataI_bgcorr <- correctBG(dataI,method="normexp")
	dataI_median <- sample.median(dataI_bgcorr)
	expr.set <- rppaList2ExpressionSet(dataI_median)
    }
}
\keyword{ manip }
