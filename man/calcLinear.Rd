\name{calcLinear}
\alias{calcLinear}
\title{
Calculates sample concentrations using linear model fit
}
\description{
calculates sample concentrations of a RPPA data set, using parameter of a linear
 model fitted to the dilution series.
}
\usage{
calcLinear(x, sample.id = c("sample", "sample.n"), dilution = "dilution"
, method = "quantreg", plot = F, detectionLimit = T)
}
\arguments{
  \item{x}{List containing background corrected RPPA data set
}
  \item{sample.id}{
character vector refering to column names from which samples can be separated
}
  \item{dilution}{
column name from the column in feature data that describes the dilution steps
of each sample
}
  \item{method}{
character string describing the method used for the linear fit
}
  \item{plot}{
logical. If true dilution curves are plotted
}
  \item{detectionLimit}{
logical. If true model is fitted on dilution steps above the detection limit.
If false, all data points are used to fit the model
}
}
\value{

\item{expression }{matrix with protein expression data}
\item{dummy }{matrix with protein expression data}
\item{arraydescription }{data frame with feature data}
\item{sampledescription }{data frame with pheno data}
}
\author{
Heiko Mannsperger <h.mannsperger@dkfz.de>,Stephan Gade <s.gade@dkfz.de>
}
\note{
for calculation of serial diluted samples only
}

\examples{
\dontrun{
library(RPPanalyzer)
data(ser.dil.samples)

predicted.data <- calcLinear(ser.dil.samples,sample.id=c("sample","sample.n"),
dilution="dilution")
}
}
\keyword{ manip }

