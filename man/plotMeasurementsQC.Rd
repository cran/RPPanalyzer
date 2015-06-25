\name{plotMeasurementsQC}
\alias{plotMeasurementsQC}
\title{ Scatter Plots from an RPPA data }
\description{
  Plots the blank signals and the target specific signals of an RPPA data list
  in a PDF file.
}
\usage{
   plotMeasurementsQC(x, file = "QC_plots.pdf", arrays2rm = c("protein"))
}
\arguments{
  \item{x}{ RPPA data list as output from \code{read.Data} }
  \item{file}{ name of the PDF file that will be exported }
  \item{arrays2rm}{ character describing the arrays that dont have be plotted }
}
\details{
   This function genrates scatter plots in a pdf file from not yet normalized samples
    (annotated as \code{measurement} in the \code{sample_type} column
   of the sampledescription file) of RPPA data to get an impression of the distance
   from the blank signal to the target specific signal.
   An array with blank as target description is needed.
}
\value{
   Genrates a PDF file
}
\author{ Heiko Mannsperger <h.mannsperger@dkfz.de> }
\examples{
	library(RPPanalyzer)
	data(dataIII)
   plotMeasurementsQC(dataIII,file="control_plot.pdf")
}

\keyword{ hplot }
