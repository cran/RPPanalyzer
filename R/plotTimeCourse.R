`plotTimeCourse` <-
function (x
    ,tc.identifier=c("sample","stimulation","inhibition","stim_concentration")
    ,plot.split="experiment",file="splineplot.pdf"
    ,arrays2rm=c("protein","Blank"),plotformat="rawdata") {


    data <- select.measurements(x)

    ## remove arrays without (biological) information
    data <- remove.arrays(data,param="target",arrays2rm=arrays2rm)

    ## create a column to identify the time courses that should printed in one plot
    datax <- create.ID.col(data,sample.id=tc.identifier)


    plotcount <- levels(as.factor(datax[[4]][,plot.split]))

    pdf(file=file)


    for ( i in seq(along=plotcount)){

        tempdat <- select.sample.group(datax,param=plot.split,sel=plotcount[i])

        time.series <- unique(tempdat[[4]][,"identifier"])


        for (k in 1:ncol(tempdat[[1]])){

            par(lwd=2)

            plot(tempdat[[4]][,"time"],tempdat[[1]][,k]
                ,type="n",main=c("time course",tempdat[[3]]["target",k])
                ,ylab="signal",xlab="time"
                ,ylim=c(min(tempdat[[1]][,k]),max(tempdat[[1]][,k]))
                ,xlim=c(0,max(tempdat[[4]][,"time"])))


            color <- rainbow(length(time.series))

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

