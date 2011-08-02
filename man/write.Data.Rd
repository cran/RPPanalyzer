\name{write.Data}
\alias{write.Data}
\title{
writes an RPPA data list into csv file
}
\description{
Writes the 3 or 4 elements of an RPPA data list into one or two csv files
which can easily imported into spreadsheet software
}
\usage{
write.Data(x,FileNameExtension="Data")
}
\arguments{
  \item{x}{List with RPPA data set}
  \item{FileNameExtension}{character string which will be added to the name of the
  exported file}
}
\value{
	one or two csv files dependend from the length of the RPPA data list
}
\author{
Heiko Mannsperger <h.mannsperger@dkfz.de>
}
\examples{
	library(RPPanalyzer)
	data(dataII)
	
	write.Data(dataII)
}
\keyword{ IO }
