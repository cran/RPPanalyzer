\name{test.correlation}
\alias{test.correlation}
\title{
   Tests for correlations in RPPA data
}
\description{
   Tests for correlation between protein expression value and any continuous
   data using cor.test.
}
\usage{
test.correlation(x, param, method.cor = "kendall", 
	method.padj = "BH", file = "correlation_plot.pdf") 
}
\arguments{
  \item{x}{
		List containing RPPa data set
 }
  \item{param}{
		character describing the parameter	
}
  \item{method.cor}{
      character string describing the correlation
}
  \item{method.padj}{
      character string describing the method for the p-value correction for
      multiple testing.
}
  \item{file}{
   character string
}
}
\value{
	generates a pdf file
}

\author{
	Heiko Mannsperger <h.mannsperger@dkfz.de>
}


\seealso{
   For information about the argument method.cor see \code{\link{cor.test}},
   informations about methods.padj can be found under \code{\link{p.adjust}}
   
}
\examples{
\dontrun{
library(RPPanalyzer)
data(dataIII)
dataIII_median <- sample.median(dataIII)
test.correlation(dataIII_median,param="staging")
}

}

\keyword{ hplot }
