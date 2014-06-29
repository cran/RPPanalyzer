\name{averageData}
\alias{averageData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Average biological replicates over different slides.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function assumes that each signal originates from an underlying true value which is scaled by a scaling factor depending on the slide and replicate. The method optimizes the scaling and truth parameters such that the distance between predicted and actual signals is minimized. There are aguments to specify what factors the scaling factors and truth parameters depend on.
}
\usage{
averageData(subsample, scaling = c("slide", "replicate"), 
	distinguish = c("cellline", "treatment"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subsample}{
  data.frame with columns "slide" (factor, the slide names), "ab" (factor, the antibody/target names), "time" (numeric, the time points), "signal" (numeric, signal values), "var0" (numeric, error parameter for the constant error), "varR" (numeric, error parameter for the relative error). The data.frame may contain further columns that can then be used in the \code{scaling} and \code{distinguish} arguments. The data.frame is a standard output of \code{\link{getErrorModel}}.
}
  \item{scaling}{
  character. One scaling parameter ist estimated for each occurring combination of the corresponding factors.
}
\item{distinguish}{
  character. One truth parameter ist estimaed for each occuring combination of the factors "time", "ab" (antibody/target) and the factors in \code{distinguish}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
  Averaging is based on the assumption that for each level of \code{scaling} there is an underlying "true" antibody time-course for each level of \code{distinguish}. The signals of different scaling levels are assumed to differ by a scaling factor. Both, antibody time-course values and scaling parameters are estimated simulatenously by generalized least squares estimation:
  \deqn{GRSS = \sum_{i,j}(s_i S_{ij}- y_j/s_i)^2/(\sigma_{ij,0}^2 + (y_j/s_i)^2 \sigma_{ij,R}^2)}
  where \eqn{i, j} correspond to the levels of \code{c("time", "ab", distinguish)} and the levels of \code{scaling}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{data.frame}{
  with columns "time", "ab", "signal" (the truth parameters returned by \code{nls}), "sigma" (the standard error of the truth parameter returned by \code{nls}), "connection" (integer, signals can only be compared on the same scale if they agree in "ab", and "connection") and one column for each entry of \code{distinguish}.
} 

}
\author{
%%  ~~who you are~~
Daniel Kaschek, Physikalisches Institut, Uni Freiburg. Email: \code{daniel.kaschek@physik.uni-freiburg.de}
}
