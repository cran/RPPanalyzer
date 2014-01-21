\name{plotTimeCourse}
\alias{plotTimeCourse}
\title{ Draw time course from RPPA data }
\description{
  Draws time course data from a RPPA data list and calculates a mathematical
  model on the time course data.
 }
\usage{
plotTimeCourse(x, tc.identifier = 
			c("sample", "stimulation", "inhibition", "stim_concentration"),
		tc.reference=NULL, plot.split = "experiment", file = NULL,
		arrays2rm = c("protein", "Blank"), plotformat = "stderr",
		log=TRUE, color=NULL, xlim = NULL, ylim = NULL)
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
  \item{plotformat}{ character string defining the plot type: \code{rawdata} for 
    plotting the connected medians plus standard deviation of the data, 
    \code{spline} and \code{both} for plotting the a spline fit through the data
    or both raw data and spline, as well as a confidence band showing the standard
    error of the spline fit; \code{spline_noconf} only plots the spline without
    confidence band. \code{errbar} will show the spline fit plus raw data
    without connecting the medians by a line, \code{stderr} will show 
    a less crowded version of the spline plus standard error represented 
    as simple error bars (which is the default). }
  \item{log}{logical, if true time courses signal intensities will be plotted
  at log2 scale}
  \item{color}{ Vector holding the colors for the samples to be plot. If NULL, colors will be generated. }
  \item{xlim}{ Limits for x-axis. If NULL (default) limits are generated for each timeseries plot. 
              If a range (numeric vector of length 2) is given, this is used for all plots. }
  \item{ylim}{ Analogous to \code{xlim} for y-axis limits. }
}
\details{
  This function plots RPPA time course experiments from data sets with aggregated
  replicate spots. A column \code{time} containing numeric values is required in the sampledescription file.
  One or several column in the sampledescription file should be
  able to indentify the individual experiments described in argument \code{tc.identifier}.
  One column should provide a parameter \code{plot.split} to split the whole data set into different
  comparable time courses that have to be plotted together.\cr
  
  Different plotting options can be specified with the argument \code{plotformat}. Option \code{both} is 
  most informative, since it shows the original data plus standard deviations
  at each time point, combined with a spline fit and the standard error
  of the fit.
}
\value{
 generates a PDF file}
\author{Heiko Mannsperger <h.mannsperger@dkfz.de }
\examples{
\dontrun{
	library(RPPanalyzer)
	data(dataII)
	plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="stderr")
    plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="errbar")
    plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="both")
    plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="rawdata")
    plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="spline")
    plotTimeCourse(dataII,
		tc.identifier=c("sample","stimulation","stim_concentration","inhibition")
        ,plot.split="experiment",plotformat="spline_noconf")
               
}
}

\keyword{hplot }

