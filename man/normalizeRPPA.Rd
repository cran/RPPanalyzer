\name{normalizeRPPA}
\alias{normalizeRPPA}
\title{ Normalizes data in an RPPA data list }
\description{
  Normalizes data in an RPPA data list. Four different normalization methods
  are provided: using externally measured protein concentration, signals from
  housekeeping proteins or protein dyes and row normalization.
}
\usage{
normalizeRPPA(x, method = "row", normalizer = "housekeeping", useCol = "BCA",
writetable = F,vals="logged")
}
\arguments{
  \item{x}{ List containing RPPA data set }
  \item{method}{ character string: one of \code{proteinDye},\code{row},
  \code{housekeeping},\code{extValue}   }
  \item{normalizer}{ character string describing the target in slidedescription
  that should be used for normalization using \code{housekeeping} }
  \item{useCol}{ character string describing the column in sampledescription
  that should be used for normalization using the method \code{extValue}. }
  \item{writetable}{ logical. If true data are exported as tab delimited text
  files to current working directory }
  \item{vals}{ the data is returned at log2 scale with substracted normalizer
  value per default. If argument is set to \code{native} the median of the normalizer
  values is added after normalization and the data is returned at native scale.}
}
\details{
  The function provides four different methods to normalize RPPA data to ensure
   that an optimal data quality. The default method \code{row} uses the expression
   matrix: after taking the logarithm the row median is substracted from each
   value of one row assuming that the median expression over all targets of one
   sample is representing total protein amount of the spots. For the method
   \code{proteinDye} arrays with the pattern \code{protein} in the target
   description are used for normalization. For every spotting run a separate
   protein slide is required. If the slides containing more than one array, the
   arrays will be normalized by the corresponding \code{protein} array. To use
   external protein assay data for normalization, a column containing the protein
   concentration has to be added to the sampledescription file. The name of this
   column is addressed via the \code{useCol} argument. To use any other target
   for normalization the method \code{housekeeping} can be used. The target
   for this method has to be addressed via the \code{normalizer} argument.
   
}
\value{

  \item{expression }{matrix with protein expression data}
   \item{dummy}{matrix with protein expression data}
   \item{arraydescription }{data frame with feature data}
   \item{sampledescription }{data frame with pheno data}
}
\author{Heiko Mannsperger <h.mannsperger@dkfz-heidelberg.de>}
\examples{
\dontrun{
	library(RPPanalyzer)
	data(dataI)
	dataI_bgcorr <- correctBG(dataI,method="normexp")
	dataIb <- pick.high.conc(dataI_bgcorr,highest="dilution")
	normRow <- normalizeRPPA(dataIb,method="row")
	normDye <- normalizeRPPA(dataIb,method="proteinDye")
	normPassay <- normalizeRPPA(dataIb,method="extValue",useCol="concentration")
	normHK <-  normalizeRPPA(dataIb,method="housekeeping",normalizer="housekeeping")
}
}
\keyword{manip }
