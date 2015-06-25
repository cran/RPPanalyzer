\name{plotqq}
\alias{plotqq}
\title{ qq-plot and qq-line of an RPPA data set }
\description{
  Draws a qq-plot and qq-line from measurements samples of a RPPA data set
}
\usage{
   plotqq(x, fileName = "qqplot_and_line.pdf")
}

\arguments{
  \item{x}{ RPPA data list as output from \code{read.Data} }
  \item{fileName}{ name of the PDF file }

}
\details{
  This function implements the functions \code{qqnorm} and \code{qqline} from
  stats package to get an impression of the data distribution in an RPPA data set.
}
\value{
 generates a PDF file.
}
\author{ Heiko Mannsperger <h.mannsperger@dkfz.de> }
\examples{
	library(RPPanalyzer)
	data(dataIII)
	plotqq(dataIII,file="dataIII_qqplot.pdf")
    }
\keyword{hplot }

