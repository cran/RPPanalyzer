\name{rppaList2Heatmap}
\alias{rppaList2Heatmap}

\title{
Draw a heatmap with column side colors from a RPPA data
}
\description{
	Draws a heatmap from an RPPA data set and adds column side colors
   visualizing groups of selected phenodata.
}
\usage{
   rppaList2Heatmap(x, sampledescription = "sample", side.color = "tissue",
   remove = c("blank", "protein", "Abmix"), distance = "eucsq",
   dendros = "both", cutoff = 0.005, fileName = NULL,
   cols = colorpanel(100, low = "blue", mid = "yellow", high = "red"),
   hclust.method="ward", scale = "row")
}
\arguments{
  \item{x}{
		List with RPPA data set, aggregatedreplicates 
}
  \item{sampledescription}{
		character describing the sample identifier
}
  \item{side.color}{
		character describing the parameter for the side colors of the heatmap
}
  \item{remove}{
		character describing the arrays that should removed from the heatmap data
}
  \item{distance}{
		character describing the method for the dendrogram
}
  \item{dendros}{
 		character: "both" for row and column dendrogram
}
  \item{cutoff}{
		numeric describing the percentage that are identified as outliers for the
      heatmap color distribution
}
  \item{fileName}{
		character for the file where the pdf file will be stored. If NULL,
		plot to standard plotting device.
}
  \item{cols}{
		color key for the heatmap	
}
  \item{hclust.method}{
	The method to be used for cluster agglomeration. Defaults to \code{ward}.
	See help of \code{hclust} for options.
}
  \item{scale}{
    String. Either \code{row, column, both or none} for row or column, both or no scaling, respectively.
}
}

\value{
   generates a PDF file
}

\author{
   Heiko Mannsperger <h.mannsperger@dkfz.de>
}

\examples{
	library(RPPanalyzer)
	data(dataIII)
	dataIII_median <- sample.median(dataIII)
	
	rppaList2Heatmap(dataIII_median)
}

\keyword{ hplot }
