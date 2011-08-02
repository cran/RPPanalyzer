\name{rppa2boxplot}
\alias{rppa2boxplot}
\title{Draws boxplots of groups of an RPPA data set including wilcox or kruskal test.}
\description{
 Draws boxplots of groups of an RPPA data set and compares the expression values to a reference group (control) if provided (wilcox.test). Otherwise a test on general differences is performed (kruskal.test). Additionally a grouping order for plotting can be provided here.
 }
\usage{
rppa2boxplot(x, param, control=NULL, orderGrp=NULL, file = "boxplot_groups.pdf")
}
\arguments{
  \item{x}{ List with RPPA data with aggregated replicate spots }
  \item{param}{ Character value of one of the columns of the sampledescription matrix, i.e. x[[4]], describing the phenodata that should be analyzed }
  \item{control}{Character value of one of the columns of the sampledescription matrix, i.e. x[[4]], describing the sample group of \code{param} that serves as reference in the wilcoxon test. In case of NULL (default) the general kruskal.test is performed instead. }
  \item{orderGrp}{ defines the ordering of the subgroups in \code{param}, i.e. vector of specifically ordered values of \code{param} }
  \item{file}{ Title of the file that will be exported.  }
}

\value{
  Generates a PDF file
}
\author{Silvia von der Heyde, Heiko Mannsperger}
\examples{
\dontrun{
library(RPPanalyzer)
	
data(dataIII)
dataIII_median <- sample.median(dataIII)
rppa2boxplot(x=dataIII_median, param="rank", control="vx",
	orderGrp=c("vx","zx","yzr","rxi"), file="wilcoxonBoxplot.pdf")
rppa2boxplot(x=dataIII_median, param="rank", control=NULL,
	orderGrp=c("vx","zx","yzr","rxi"), file="kruskalBoxplot.pdf")
}
}

\keyword{ hplot }

