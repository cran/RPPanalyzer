\name{slidedescription}
\alias{slidedescription}
\docType{data}
\title{ slide description file}
\description{

    The slide description file contains all information concerning the arrays
    of a reverse phase protein experiment.

}

\format{

  tab delimeted text file
}
\details{

   The slide description file contains information for array annotation and data analysis.
   To find the GenePix result files (gpr files) in current working directory it
   is neccesssary that the names of the gpr files are matching with the \code{gpr} column.
   To identify the array on the slides the columns \code{pad}, \code{slide}, \code{spotting_run}, \code{incubation_run}
   are obligatory. It is neccessary that every well that is spottet is described.
   The columns \code{sample_type} and \code{sample} as well as \code{concentration} and
   (for serially diluted samples) \code{dilution} are required for data analysis.
   The columns \code{target} describes the analyzed proteins and \code{AB_ID}
   contains a indentifier for the antibody used for the detection.
   Any additionally column can be added to describe further phenodata of interest.

}
\source{

 The data set contains the incubation data from reverse phase protein arrays
 with randomized feature data.
}

\keyword{datasets}
