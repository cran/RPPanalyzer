\name{rppa2boxplot}
\alias{rppa2boxplot}
\title{ Draw boxplots of an RPPA data set. }
\description{
 Draws boxplots of groups of an RPPA data set and compares the expression values to a reference group.
}
\usage{
rppa2boxplot(x, param = "tissue", wilcoxtest = FALSE, control = "normal", file = "boxplot_groups.pdf")
}
\arguments{
  \item{x}{ List with RPPA data with aggregated replicate spots }
  \item{param}{ character describing the phenodata that should be analyzed }
  \item{wilcoxtest}{ logical. If true, a wilcoxon test is performed on the groups found in \code{param}. }
  \item{control}{Character describing the sample group of \code{param} that serves as reference in the wilcoxon test. }
  \item{file}{ Title of the file that will be exported.  }
}

\value{
  Generates a PDF file
}
\author{Heiko Mannsperger <h.mannsperger@dkfz.de>}
\examples{
	library(RPPanalyzer)
	
	data(dataIII)
	dataIII_median <- sample.median(dataIII)
	rppa2boxplot(dataIII_median,param="rank",control="vx",wilcoxtest=TRUE)

}

\keyword{ hplot }

