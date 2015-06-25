\name{simpleBoxplot}
\alias{simpleBoxplot}
\title{Draws boxplots of groups of an RPPA data set.}
\description{
 Draws boxplots of groups of an RPPA data set. Additionally a grouping order for plotting can be provided here.
 }
\usage{
simpleBoxplot(x, param, orderGrp=NULL, file = "boxplot_groups.pdf")
}
\arguments{
  \item{x}{ List with RPPA data with aggregated replicate spots }
  \item{param}{ Character value of one of the columns of the sampledescription matrix, i.e. x[[4]], describing the phenodata that should be analyzed }
  \item{orderGrp}{ defines the ordering of the subgroups in \code{param}, i.e. vector of specifically ordered values of \code{param} }
  \item{file}{ Title of the file that will be exported.  }
}

\value{
  Generates a PDF file
}
\author{Silvia von der Heyde, Heiko Mannsperger}
\examples{
library(RPPanalyzer)
	
data(dataIII)
dataIII_median <- sample.median(dataIII)
simpleBoxplot(x=dataIII_median, param="rank", 
	orderGrp=c("vx","zx","yzr","rxi"), file="simpleBoxplot.pdf")
    }

\keyword{ hplot }

