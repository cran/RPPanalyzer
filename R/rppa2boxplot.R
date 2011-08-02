`rppa2boxplot` <-

function (x,param="tissue",wilcoxtest=FALSE,control="normal"
            ,file="boxplot_groups.pdf")                     {


if (wilcoxtest){

   wilcox.plot.groups(x,param=param,control=control,file=file)
   
   } else {

    boxplot.groups(x,param=param,file=file)
    
    }
    
}

