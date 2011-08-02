\name{calcSuperCurve}
\alias{calcSuperCurve}

\title{ Calculates the concentration of serial diluted samples}

\description{
Calculates the protein concentration of a serial diluted sample stored in an RPPA data list using quantification methods implemented in the SuperCurve package (Coombes et. al, 2009). 
}

\usage{
calcSuperCurve(x,model="cobs", method="nlrq", sample.id=c("sample","sample.n"),sel=c("measurement","control"), dilution="dilution", block.design, plot=T)
}

\arguments{
  \item{x}{RPPA data list with replicates aggregated with median }
  \item{model}{The model used by the SuperCurve package. Has to be either "cobs", "logistic" or "loess".}
  \item{method}{The method used for fitting the model. Has to be "nlrq", "nlrob" or "nls".}
  \item{sample.id}{Attributes to identify the samples}
  \item{sel}{The sample type that should be calculated. Has to be "measurements","control", "neg_control",or "blank".}
  \item{dilution}{Name of the column in the feature data matrix describing the dilution steps of the samples.}
  \item{block.design}{Vector with two elements. Describes how the blocks are arranged on the array. The first element specifies the number of rows, the second the number of columns. The blocks are supposed to be arranged row by row in the grid.}
  \item{plot}{Logical. If true, model fits are plotted }
}

\details{
This method is wrapper function to give the user access to the quantification methods implemented in the SuperCurve package. The package can be installed from \url{http://bioinformatics.mdanderson.org/Software/OOMPA/}.
}

\value{

  \item{expression}{matrix with expression values}
  \item{error}{matrix with error values}
  \item{arraydescription}{data frame with feature data}
  \item{sampledescription}{data frame with pheno data}
}

\references{Coombes et. al, 2009, SuperCurve: SuperCurve Package

Hu et al., Bioinformatics 2007, Non-parametric quantification of protein lysate arrays
}

\author{Stephan Gade <s.gade@dkfz.de> }
\examples{

library(RPPanalyzer)
data(ser.dil.samples)

\dontrun{predicted.data <- calcSuperCurve(ser.dil.samples, sel=c("measurement"), block.design=c(1,4))}
}
\keyword{ manip }
