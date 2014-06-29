\name{getErrorModel}
\alias{getErrorModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estimates error model parameters var0 (basal variance) and varR (relative variance) and produces a new data.frame with the signals and error model parameters.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The method is based on a maximum-likelihood estimation. The model prediction is the expected variance given the signal, depending on var0 and varR.
}
\usage{
getErrorModel(dataexpression, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataexpression}{data.frame, standard output from RPPanalyzer's \code{\link{write.Data}}.}
  \item{verbose}{logical, if TRUE, the function prints out additional information and produces a PDF file in the working directory with the signal vs. variance plots.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The empirical variance estimator is \eqn{\chi^2} distributed with \eqn{n-2} degrees of freedom, where \eqn{n} is the number of technical replicates. The estimated error parameters maximize the corresponding log-likelihood function. At the moment, the code assumes \eqn{n=3}. For cases \eqn{n>3}, the error parameters are slightly overestimated, thus, providing a conservative result. The explicit error model is
\deqn{\sigma^2(S) = \sigma_0^2 + S^2\sigma_R^2 = var0 + varR S^2}
where \eqn{S} is the signal strength.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{data.frame}{with columns "slide" (factor, the slide names), "ab" (factor, the antibody/target names), "time" (numeric, the time points), "signal" (numeric, signal values), "var0" (numeric, error parameter for the constant error, equivalent to sigma0^2), "varR" (numeric, error parameter for the relative error, equivalent to sigmaR^2) and other columns depending on the input data.frame}
}
\author{
%%  ~~who you are~~
Daniel Kaschek, Physikalisches Institut, Uni Freiburg. Email: \code{daniel.kaschek@physik.uni-freiburg.de}
}

