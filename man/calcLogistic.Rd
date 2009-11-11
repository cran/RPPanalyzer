\name{calcLogistic}
\alias{calcLogistic}
\title{
Calculates sample concentrations using sigmoid model fit
}
\description{
Calculates sample concentrations of a RPPA data set, as wrapper for
curveFitSigmoid.
}
\usage{
calcLogistic(x, sample.id = c("sample", "sample.n"), dilution = "dilution",
xVal = NULL, plot = F, detectionLimit = F)
}

\arguments{
  \item{x}{
    \code{x} List containing RPPA data set
}
  \item{sample.id}{
character vector refering to column names from which samples can be separated
}
  \item{dilution}{
column name from the column in feature data that describes the dilution steps
of each sample
}

  \item{xVal}{
defines the dilution value for which the concentration is calulated. If null
the highest dilution value is used
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
\item{expression_b }{matrix with protein expression data}
\item{arraydescription }{data frame with feature data}
\item{sampledescription }{data frame with pheno data}
}

\author{
Heiko Mannsperger <h.mannsperger@dkfz.de>, Stephan Gade <s.gade@dkfz.de>
}

\examples{

library(RPPanalyzer)
data(subsetdataI)

datlogistic <- calcLogistic(subsetdataI,sample.id=c("sample","sample.n")
                            ,dilution="dilution")
}                            
\keyword{ manip }
