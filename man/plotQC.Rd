\name{plotQC}
\alias{plotQC}
\title{ Plot target and blank signal from RPPA control samples}
\description{
  Plots target and blank signal from control samples of an RPPA data set in one plot.
  Exports pdf file.
}
\usage{
plotQC(x, file = "target_vs_blank.pdf", arrays2rm = c("protein"))
}
\arguments{
  \item{x}{ RPPA data list as output from \code{read.Data} }
  \item{file}{ name of the PDF file }
  \item{arrays2rm}{ character describing the arrays that dont have be plotted }
}
\details{
   This function genrates scatter plots in a pdf file from not yet normalized, serially diluted
   control samples (annotated as \code{control} in the \code{sample_type} column
   of the sampledescription file) of RPPA data to get an impression of the antibody dynamic. An
   array with blank as target description is needed.
}
\value{
  generates a PDF file
}
\author{ Heiko Mannsperger <h.mannsperger@dkfz.de>}
\examples{
library(RPPanalyzer)
data(dataIII)

plotQC(dataIII,file="plotQC.pdf")
}
\keyword{ hplot }
