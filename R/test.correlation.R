`test.correlation` <-
function (x,param="concentration",method.cor="kendall",method.padj="BH"
            ,file="correlation_plot.pdf"){

	data <- select.measurements(x)
	

	###############################################
	## first loop for calculating adjusted p-values

	pvals <- c(NULL)
	tauvals <- c(NULL)


		for (k in 1:ncol(data[[1]])){

			rval <- cor.test(data[[1]][,k],data[[4]][,param],method=method.cor)

			pvals <- c(pvals,rval[[3]][1])
			
			tauvals <- c(tauvals,rval[[4]][1])
			
			}
	## correct p-values for multiple testings
	adjustedp <- p.adjust(pvals,method=method.padj)

	###########################
	## second loop for plotting
	count <- 0

   pdf(file=file)

 	for (k in 1:ncol(data[[1]])){

			count <- count+1
         par(lwd=2)
				plot(data[[1]][,k],data[[4]][,param]
				, xlab=paste("log",data[[3]]["target",k],"expression")
            , ylab= param
				, main=c(paste("correlation of",data[[3]]["target",k],"expression")
                     ,paste("to",param)
                     ,paste("correlation:  ",round(tauvals[count],digits=3),"  "
					             ,"p-value:  ",signif(adjustedp[count],digits=4)))
				, pch=2,col="red")

			abline(lm(data[[4]][,param]~data[[1]][,k]),col="darkgreen")

			}
			
			dev.off()
		}

