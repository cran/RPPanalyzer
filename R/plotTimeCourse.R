`plotTimeCourse` <- function (x
    ,tc.identifier=c("sample","stimulation","inhibition","stim_concentration")
    ,tc.reference=NULL
    ,plot.split="experiment",file="splineplot.pdf"
    ,arrays2rm=c("protein","Blank"),plotformat="rawdata"
    ,log=T) {

    ## select the measurements in the data
    data <- select.measurements(x)

    ## remove arrays without (biological) information
    data <- remove.arrays(data,param="target",arrays2rm=arrays2rm)

    ## create a column to identify the time courses that should printed in one plot
    datax <- create.ID.col(data,sample.id=tc.identifier)

    ### if a reference is given
    #if(!is.null(y)) {

    ### select measurements and remove the control arrays
    #reference <- select.measurements(y)
    #reference <- remove.arrays(reference, param="target", arrays2rm=arrays2rm)

    ## check if the same arrays are available in the reference
    #m <- match(colnames(x[[1]]), colnames(reference[[1]]), nomatch=0)

    #if (any(m==0)) {
    #stop("Some of the arrays in the data are not available in the reference!")
    #} 

    #}


    plotcount <- unique(datax[[4]][,plot.split])

    pdf(file=file)

    for ( i in seq(along=plotcount)){

        #tempdat <- select.sample.group(datax,param=plot.split,sel=plotcount[i])
        groupFactor <- list(plotcount[i])
        names(groupFactor) <- plot.split
        tempdat <- select.sample.group(datax, params=groupFactor)


        ## order data subset
        order.tc <- order(tempdat[[4]][,"identifier"],tempdat[[4]][,"time"])
        tempdat[[1]] <- tempdat[[1]][order.tc,]
        tempdat[[2]] <- tempdat[[2]][order.tc,]
        tempdat[[4]] <- tempdat[[4]][order.tc,]
        


        # create the identiefiers for the different time series in one plot

        time.series <- unique(tempdat[[4]][,"identifier"])
    
        # create a nice looking color vector
        color <- rainbow(length(time.series))

        # iterate over all arrays/proteins
        for (k in 1:ncol(tempdat[[1]])){



            # if a reference is given
            if(!is.null(tc.reference)) {

                # get the ID for the reference time series
                refID <- paste(tc.reference, collapse="")

                # search the time series in the data
                refLines <-  tempdat[[4]][,"identifier"]==refID

                # get the zero time point of the time series
                refLines & tempdat[[4]][,"time"]==0

                # check there data available, that means if the combination of parameters was valid for this split
                # if it was, than calculate the reference value
                # otherwise set the reference value to zero or one, depending on the log status
                if(!any(refLines) && log) {

                    ref <- 0

                }
                else if(!any(refLines) && !log) {

                    ref <- 1

                }
                else {

                    ref <- median(tempdat[[1]][refLines & tempdat[[4]][,"time"]==0,k], na.rm=T)

                }
            }

	    # divide by the reference
	    if(!is.null(tc.reference) && log) {

                    tempdat[[1]][, k] <- tempdat[[1]][, k] - ref
                    tempdat[[2]][, k] <- tempdat[[2]][, k] - ref



	    }
	    else if(!is.null(tc.reference) && !log) {

                    tempdat[[1]][, k] <- tempdat[[1]][, k]/ref
                    tempdat[[2]][, k] <- tempdat[[2]][, k]/ref

	    }

            par(lwd=2)

            # create the plotting area with a simple plot
            plot(tempdat[[4]][,"time"],tempdat[[1]][,k]
                    ,type="n",main=c(paste("time course: ",plotcount[i]),tempdat[[3]]["target",k])
                    ,ylab="signal",xlab="time"
                    ,ylim=c(min(tempdat[[1]][,k]),max(tempdat[[1]][,k]))
                    ,xlim=c(0,max(tempdat[[4]][,"time"])))

            for (j in seq(along=time.series)) {


                time.lines <- which(tempdat[[4]][,"identifier"]==time.series[j])


                expr.data <- cbind(tempdat[[4]][time.lines,"time"]
                        ,tempdat[[1]][time.lines,k]
                        ,tempdat[[2]][time.lines,k]
                        )


                colnames(expr.data) <- c("time",tempdat[[3]]["target",k],"error")
                expr.data <- expr.data[order(expr.data[,"time"]),]


                if (plotformat=="rawdata" | plotformat=="both"){

                    errbar(expr.data[,"time"],expr.data[,2]
                            ,expr.data[,2]+expr.data[,3]
                            ,expr.data[,2]-expr.data[,3]
                            ,col=color[j],add=T)

                    lines(expr.data[,"time"],expr.data[,2],col=color[j],lty="dashed",lwd=1)
                }

                if (plotformat=="spline" | plotformat=="both"){
                    y <- expr.data[,2]
                    tp <- expr.data[,1]
                    splinemodel <- gam(y~s(tp))

                    # x-vector for the predictions
                    xn=seq(0,max(tp),0.1)

                    ## add spline to plot
                    lines(xn,predict(splinemodel,newdata=data.frame(tp=xn)),
                            col=color[j],lty="solid",lwd=2)
                }
            }

            legend("topleft",col=color,legend=time.series
                    ,pch=rep(1,length(time.series))
                    ,cex=0.5,lwd=1)

        }
    }
    dev.off()
}

