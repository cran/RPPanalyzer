`rppaList2Heatmap` <-
function (x,sampledescription="sample",side.color="tissue"
			, remove=c("blank","protein","Abmix"),distance = "euclidean"
         , dendros="both", cutoff=0.005, fileName="Heatmap.pdf"
         , cols=colorpanel(100, low="blue",mid="yellow",high="red")){

         stopifnot(require(gplots))
         
   data <- select.measurements(x)

	mat <- data[[1]]

	rownames(mat) <- data[[4]][,sampledescription]
	colnames(mat) <- data[[3]]["target",]

	sel.cols <- which(is.na(match(colnames(mat),remove)))

	mat <- mat[,sel.cols]
	
	groups <-unique(data[[4]][,side.color])

   rsc <- match(data[[4]][,side.color],groups)

	colors <- rainbow(length(groups))

	for (i in seq(along=groups)){

	rsc[rsc==i]=colors[i]

		}
		
		pdf(file=fileName)
		
      plot.heatmap(t(mat), distance = distance, dendros=dendros, cutoff=cutoff
                  , toFile=F, fileName=fileName
                  , cols=cols
                  , ColSideColors=rsc)

     legend(x=0,y=0.8,legend=groups,col=colors,pch=15)
     
     dev.off()
                  
   }

