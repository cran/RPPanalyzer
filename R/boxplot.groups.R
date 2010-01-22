`boxplot.groups` <-
function (x,param="tissue",file="boxplot_groups.pdf"){
	
	## select measurements from array data list
	data <- select.measurements(x)
	
	## identify groupnames of selected parameter
	 groups <-unique(data[[4]][,param])
	 
pdf(file=file)

for ( i in 1:ncol(data[[1]])){

	## generate list
	grouplist <- vector("list",length(groups))

		for (j in seq(along=groups)){

		grouplist[[j]] <- data[[1]][which(data[[4]][,param]==groups[j]),i]

		}
	names(grouplist) <- groups

	boxplot(grouplist
         ,main=c("target: ",data[[3]]["target",i]))

   stripchart(grouplist
               ,add=T,vertical=T,method="jitter",jitter=0.3,col="red")


}
dev.off()

}

