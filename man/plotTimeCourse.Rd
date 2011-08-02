\name{plotTimeCourse}
\alias{plotTimeCourse}
\title{ Draw time course from RPPA data }
\description{
  Draws time course data from a RPPA data list and calculates a mathematical
  model on the time course data.
 }
\usage{
plotTimeCourse(x, tc.identifier = c("sample", "stimulation", "inhibition", "stim_concentration"),
tc.reference=NULL, plot.split = "experiment", file = "splineplot.pdf",
arrays2rm = c("protein", "Blank"), plotformat = "rawdata",log=T)
}
\arguments{
  \item{x}{ List containing RPPA data set }
  \item{tc.identifier}{ character string describing the column names in the
  sampledescription that identifies the individual time course experiments}
  \item{tc.reference}{ character string describing the sample that will be used
  as reference for the time course plots.}
  \item{plot.split}{ character string describing the column names in
  sampledescription that defines the argument that devides between different plots }
  \item{file}{ character string for the name of the exported file }
  \item{arrays2rm}{ character strings identifying the targets that should be
  from the time course plots }
  \item{plotformat}{ character string defining the plot type: \code{rawdata},
  \code{splines},\code{both}. }
  \item{log}{logical, if true time courses signal intensities will be plotted
  at log2 scale}
}
\details{
  This function plots RPPA time course experiments from data sets with aggregated
  replicate spots. A column \code{time} containing numeric values is required in the sampledescription file.
  One or several column in the sampledescription file should be
  able to indentify the individual experiments described in argument \code{tc.identifier}.
  One column should provide a parameter \code{plot.split} to split the whole data set into different
  comparable time courses that have to be plotted together.
}
\value{
 generates a PDF file}
\author{Heiko Mannsperger <h.mannsperger@dkfz.de }
\examples{
	library(RPPanalyzer)
	data(dataII)
	plotTimeCourse(dataII,tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
               ,plot.split="experiment",file="output_ptc_x.pdf",plotformat="both")

}

\keyword{hplot }

