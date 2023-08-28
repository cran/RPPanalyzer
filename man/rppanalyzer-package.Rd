\name{RPPanalyzer-package}
\alias{RPPanalyzer-package}
\alias{RPPanalyzer}
\docType{package}
\title{
Read, annotate and normalize reverse phase protein array data and get a brief overview on the biological impact.
}
\description{
	The package reads pheno and feature data of an RPPA experiment from textfiles and annotates the expression values in genepix result files (gpr files). For background correction the \code{backgroundcorrect} funktion from the limma package is used. After normalization data can be plotted to check quality control or to get a first impression on the biological relevance of the data set.
}
\author{
Maintainer: Heiko Mannsperger <h.mannsperger@dkfz.de>

}
\keyword{ package }
\examples{
\dontrun{
	data(dataI)
	
	bgcorrected <- correctBG(dataI)
	normalized <- normalizeRPPA(bgcorrected,method="proteinDye")
	aggregated <- sample.median(normalized)
}
}
