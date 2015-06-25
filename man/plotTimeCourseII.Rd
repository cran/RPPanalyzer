\name{plotTimeCourseII}
\alias{plotTimeCourseII}
\title{
    Multiplot function for RPPA time course datasets
}
\description{
    plotTimeCourseII creates multiplot rectangular PDF files for time course datasets. Page layout (number of plots per page, arrangement of plots) and plot layout can be customized within the function.  
}
\usage{
    plotTimeCourseII(x,plotgroup="",filename="timeseries_multiplot.pdf",numpage=4,
	cols=2,xname="time",yname="signal",legpos="top",legrow=2,legtitle="treatment",
	legtitlepos="top",legtextsize=10,legtextcolor="black",legtitlesize=10,
	legtitlecolor="black",legtitleface="bold",legitemsize=1,plottitlesize=12,
	plottitleface="bold",xaxissize=10,yaxissize=10,xaxisface="bold",
	yaxisface="bold",xaxistextsize=8,xaxistextangle=0,yaxistextsize=8,
	linecolor="Set1")
}
\arguments{
    \item{x}{
        RPPA time course dataset preprocessed with the getErrorModel and averageData function
    }
    \item{plotgroup}{
        select the feature (eg. treatment) which should be plotted in one plot
    }
    \item{filename}{
        enter filename, DIR needs to be defined as your working directory, add .pdf to filename
    }
    \item{numpage}{
        number of plots per page
    }
    \item{cols}{
        number of plot columns per page
    }
    \item{xname}{
        title of the x axis
    }
    \item{yname}{
        title of the y axis
    }
    \item{legpos}{
        postion of the legend in context of the plot ("top","bottom","right","left"), "none" removes legend from plot
    }
    \item{legrow}{
        number of item rows within the legend
    }
    \item{legtitle}{
        title of the legend
    }
    \item{legtitlepos}{
        position of the legend title
    }
    \item{legtextsize}{
        font size of the legend text
    }
    \item{legtextcolor}{
        color of the legend text
    }
    \item{legtitlesize}{
        font size of the legend title
    }
    \item{legtitlecolor}{
        color of the legend title
    }
    \item{legtitleface}{
        font face of the legend title (eg. "bold")
    }
    \item{legitemsize}{
        size of the legend item pictures
    }
    \item{plottitlesize}{
        size of the plot title
    }
    \item{plottitleface}{
        font face of the plot title
    }
    \item{xaxissize}{
        font size of the x axis title
    }
    \item{yaxissize}{
        font size of the y axis title
    }
    \item{xaxisface}{
        font face of the x axis title
    }
    \item{yaxisface}{
        font face of the y axis title
    }
    \item{xaxistextsize}{
        font size of the x axis text
    }
    \item{xaxistextangle}{
        angle of the x axis text
    }
    \item{yaxistextsize}{
        font size of the y axis text
    }
    \item{linecolor}{
        color of the plot lines: either chose a scheme ("Set1","Dark2","Paired") or hand a vector of color names
    }
}
\details{
  The plotTimeCourseII function plots RPPA timecourse datasets in multiple line charts. For each cell line
	and target protein a separate plot is created. The average foldchange values of different replicates and the
	error bars are visualized. In order to be visualized by the plotTimeCourseII function, the dataset needs
	to be preprocessed by the getErrorModel and averageData function from the RPPanalyzer package.  
	Additionally the plotgroup needs to be defined if it is not named ?treatment?. The remaining arguments
	are optional. 

}
\value{
    Generates a PDF file.
}
\author{
    Johannes Bues (j.bues@dkfz-heidelberg.de)
}
\examples{
# pre-process the data
dataDir <- system.file("extdata", package="RPPanalyzer")
res <- dataPreproc(dataDir=dataDir, blocks=12, spot="aushon", exportNo=2)
# remove arrays
normdat_rm <- remove.arrays(res$normdat, param="target", arrays2rm=c("protein","blank"))
# select samples and export data
sel_sampels_A549 <- select.sample.group(normdat_rm, params=list("cell_line"="A549"), combine= FALSE)
write.Data(sel_sampels_A549, FileNameExtension="HGF_sample_data_A549")
# read selected data
dataexpression_1 <- read.table("HGF_sample_data_A549expression.txt")
# use getErrorModel function
dataexpression_2 <- getErrorModel(dataexpression_1, verbose=FALSE)
# use averageData function
dataexpression_3 <- averageData(dataexpression_2, scaling=c("slide","replicate"), 
distinguish=c("cell_line","treatment"))
# plot time course data
plotTimeCourseII(dataexpression_3, filename="timecourse_HGF_sample_data_A549.pdf", 
legpos="top", xname="time [min]",
yname="signal [a.u.]", linecolor=c("red","green","blue","black","orange","grey"))
}
\keyword{file}