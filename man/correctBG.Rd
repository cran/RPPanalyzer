\name{correctBG}
\alias{correctBG}
\title{
Corrects for background in an RPPA data set
}
\description{
Corrects for background in an RPPA data set using different algorithms
(e.g. from the limma package) avoiding negative values

}
\usage{
correctBG(x, method = "normexp")
}
\arguments{
  \item{x}{
		List with RPPA data set
}
  \item{method}{
		any method from the function \code{\link[limma]{backgroundCorrect}} and \code{addmin} which adds
		a fix number to each value to avoid negative values
}
}
\details{
	This function is a wrapper for the \code{\link[limma]{backgroundCorrect}}
   function of the limma package. As additional method "addmin" is implemented. 
}
\value{
	\item{expression }{matrix with background corrected expression data}
	\item{background }{matrix with background data}
	\item{arraydescription}{data frame with feature data}
	\item{sampledescription }{data frame with pheno data}

}
\references{
Ritchie, ME, Silver, J, Oshlack, A, Holmes, M, Diyagama, D, Holloway, A,
and Smyth, GK (2007). A comparison of background correction methods for
two-colour microarrays. Bioinformatics 23, 2700-2707.
}
\author{
Heiko Mannsperger <h.mannsperger@dkfz.de>, Stephan Gade <s.gade@dkfz.de>
}

\seealso{
 For detailed information about the background correction methods see: \code{\link[limma]{backgroundCorrect}},
}
\examples{
	
library(RPPanalyzer)
data(dataI)

dataBGcorrected <- correctBG(dataI,method="normexp")

}
\keyword{ manip }
