\name{logList}
\alias{logList}
\title{
 Logarithmize (log2) the first two RPPA list elements, i.e. foreground and background signal intensities
}
\description{
Function to logarithmize (log2) the first two RPPA list elements, i.e. foreground and background signal intensities.
}
\usage{
  logList(x)
}

\arguments{
  \item{x}{list of 4 elements (\code{expression} and \code{background} data matrices, \code{arraydescription} and \code{sampledescription} data frames) according to \code{\link{read.Data}}}
}
\value{
  \item{x.log}{list of 4 elements like the input but with log2 values of \code{expression} and \code{background} matrices}
}
\author{
 Silvia von der Heyde
}
\examples{
library(RPPanalyzer)
  
# input data
dataDir<-system.file("extdata",package="RPPanalyzer")
x<-dataPreproc(dataDir=dataDir, blocks=12, spot="aushon", exportNo=4)
x.norm<-x$normdat

# get log2 list 
x.log<-logList(x.norm)
}