\name{calcSdc}
\alias{calcSdc}

\title{ Calculates the concentration of serial diluted samples}
\description{
  Calculates the protein concentration of a serial diluted sample stored in an
  RPPA data list using the serial dilution curve algorithm
  published by Zhang et.al, Bioinformatics 2009.
}
\usage{
calcSdc(x, sample.id = c("sample", "sample.n"), sel = c("measurement", "control"),
 D0 = 2, sensible.min = 5, sensible.max = 1e+09,
 minimal.err = 5, plot = T, r = 1.2)
}

\arguments{
  \item{x}{RPPA data list with replicates aggregated with median }
  \item{sample.id}{Attributes to identify the samples}
  \item{sel}{The sample type that should be calculated. Has to be "measurements","control", "neg_control",or "blank".}
  \item{D0}{Dilution factor.}
  \item{sensible.min}{Signals below this value are marked as undetected}
  \item{sensible.max}{Signals above the value are marked as saturated}
  \item{minimal.err}{Minimal valid estimate for the background noise}
  \item{plot}{Logical. If true, model fits are plotted }
  \item{r}{Constant factor used to determine the confidence interval for the saturation limit $M$ and the background noise $a$, shoul be $>1$. Can be lower if accuracy of signals is improved.}

}

\details{
The method of Zhang et. al doesn't fit the dose response curve but a derive model describing the functional relationship between the signals of two consecutive dilution steps. Since this new model does not contain the protein concentration anymore all spots of one array can be used for the fit, allowing a much more robust estimation of the underlying paramters.
}

\value{

  \item{concentration_sdc }{matrix with expression values}
  \item{error_sdc }{matrix with error values}
  \item{arraydescription_sdc }{data frame with feature data}
  \item{sampledescription_sdc }{data frame with pheno}
}
\references{Zhang et. al, Bioinformatics 2009,Serial dilution curve:
a new method for analysis of reverse phase protein array data }
\author{Heiko Mannsperger <h.mannsperger@dkfz.de>, Stephan Gade <s.gade@dkfz.de> }
\examples{

library(RPPanalyzer)
data(subsetdataI)

dataI_median <- sample.median(subsetdataI)
predicted.data <- calcSdc(dataI_median,D0=2,sel=c("measurement"))
}
\keyword{ manip }
