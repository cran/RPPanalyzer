`wilcox.plot.groups` <-
function (x,param="tissue",control="N",file="wilcox_groups.pdf"){

	## select measurements from array data list
	data <- select.measurements(x)

	## identify groupnames of selected parameter
	 groups <-unique(data[[4]][,param])

    groups.i <- as.character(groups[-which(groups==control)])
    
################################################################################
   ## generate p-vlaue using wilcoxon test
   
      l.pvals <- vector("list",ncol(data[[1]]))

      for ( i in 1:ncol(data[[1]])){

      pvals <- c(NULL)
           
    		   for (j in 1:length(groups.i)){

      	p <- wilcox.test(data[[1]][which(data[[4]][,param]==control),i]
                        ,data[[1]][which(data[[4]][,param]==groups.i[j]),i])

            pvals <- c(pvals,p$p.value)
            
      		}
        adjustedvals <- signif(p.adjust(pvals,method="BH"),digits=3)
        
        l.pvals[[i]] <- adjustedvals
        }
        names(l.pvals) <- data[[3]]["target",]
################################################################################
pdf(file=file)

groupx <- c(control,groups.i)

for ( i in 1:ncol(data[[1]])){

	## generate list
	grouplist <- vector("list",length(groupx))

		for (j in seq(along=groupx)){

		grouplist[[j]] <- data[[1]][which(data[[4]][,param]==groupx[j]),i]

		}
	names(grouplist) <- groupx

   par(lwd=2,bty="n")
	boxplot(grouplist
         ,main=c("target: ",data[[3]]["target",i]),bty="n"
         , ylab="expression level",xlab="sample groups",las=2)

   stripchart(grouplist
               ,add=T,vertical=T,method="jitter",jitter=0.3,col="red")


   text( c(1:length(groups)),c(max(unlist(grouplist))),c("control",l.pvals[[i]]), col="green")

   }
dev.off()
}

