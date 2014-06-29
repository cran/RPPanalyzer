\name{correctDilinterc}
\alias{correctDilinterc}
\alias{analyzeIntercepts}
\alias{getIntercepts}
\alias{getSignals}
\alias{as.my}
\title{
Dilution series intersect correction
}
\description{
Consists of 3 functions: \code{getIntercepts()}, \code{analyzeIntercepts()} and \code{getSignals()}.
The first one derives intercepts of dilution series in dependence of dilSeriesID (column in sampledescription.txt) and slide/pad/incubationRun/spottingRun number (colnames of arraydescription). A smoothing spline is used to extrapolate to 0. Nonparametric bootstrap is used to estimate uncertainty of the intercept estimate.
The second function is used in the last one and does Analysis of Variances for nested models.
The last one updates the original timeseries signal to (foreground expression - intercept).
}
\usage{
	correctDilinterc(dilseries, arraydesc, timeseries, exportNo) 
    getIntercepts(dilseries, arraydesc)
    analyzeIntercepts(intercepts, test="F", export)
    getSignals(timeseries, intercepts, arraydesc, exportNo)
	as.my(v)
}
\arguments{
  \item{dilseries}{foreground signal matrix as result of \code{\link{write.Data}} and import of resulting txt file, but just sample_type "control", i.e. dilution series}
  \item{arraydesc}{"arraydescription" matrix of the RPPA data set list}
  \item{timeseries}{foreground signal matrix as result of \code{\link{write.Data}} and import of resulting txt file, but just sample_type "measurement"}
  \item{exportNo}{integer of 1-4 which of the linear fits should be exported to the attribute of the result, variable for \code{analyzeIntercepts()}, 1: constant, 2: antibody, 3: antibody + slide (default) or antibody + slide + sample (\code{dilSeriesID})}
  \item{intercepts}{output of \code{getIntercepts()}, data frame with columns for dilSeriesID and slide/pad/incubationRun/spottingRun number as well as antibody, estimated intercept and estimated error of intercept}
  \item{test}{test parameter for ANOVA (see documentation of \code{\link{anova}}), default is "F"}
  \item{export}{see \code{exportNo}}
  \item{v}{some variable}
}
\value{
matrix with adapted signal intensities via subtraction of dilution intercept at concentration 0
}
\author{
Daniel Kaschek, Silvia von der Heyde
}
\examples{
\dontrun{
library(RPPanalyzer)
# read data
dataDir <- system.file("extdata", package="RPPanalyzer")
setwd(dataDir)
rawdata <- read.Data(blocksperarray=12, spotter="aushon", printFlags=FALSE)
# write data
write.Data(rawdata,FileNameExtension="test_data")
# import raw data
fgRaw.tmp <- read.delim("test_dataexpression.txt",\cr
stringsAsFactors=FALSE, row.names=NULL, header=TRUE)
fgRaw <- read.delim("test_dataexpression.txt", skip=max(which(fgRaw.tmp[,1]==""))+1,\cr
stringsAsFactors=FALSE, row.names=NULL, header=TRUE)
# remove NAs
fgNAVec <- which(is.na(fgRaw[,"ID"]))
if(length(fgNAVec) > 0){
fgRaw <- fgRaw[-fgNAVec,]
}
colnames(fgRaw) <- sub("X","", gsub("\\.","-", colnames(fgRaw)))
# correct data for BG noise
correctedData <- correctDilinterc(dilseries=fgRaw[which(fgRaw$sample_type=="control" &\cr
!is.na(fgRaw$dilSeriesID)),], arraydesc=rawdata$arraydescription,\cr
timeseries=fgRaw[which(fgRaw$sample_type=="measurement"),], exportNo=2)
}
}
