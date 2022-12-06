require(ggplot2)
require(plyr)
require(data.table)


get.plot.highlight<-function(data,l.main=NULL,highlight=NULL,highlight.col="darkblue",l.y.max=NULL,l.y.min=0,l.yl.labs=expression(-log[10](italic("P")))){
  if (is.null(l.y.max)) {
    l.y.max<-ceiling(max(data$value))+10}
  data$chr<-as.factor(data$chr)
  g <- ggplot(data, aes(x=pos, y=value,col=chr))
  g<-g+geom_point()
  if(!is.null(highlight)){
    g<-g+geom_point(data=highlight,color=highlight.col)
  }
  g<-g+ylim(l.y.min,l.y.max)
  g<-g+facet_grid(.~chr,scale="free_x",space = "free_x")
  if(nlevels(data$chr)%%2==1) { v<-rep(c("black","grey"),ceiling(nlevels(data$chr)/2))
  v<-v[-length(v)]} else {
    v<-rep(c("black","grey"),nlevels(data$chr)/2)
  }
  g <- g + scale_color_manual(values = v, guide=F)
  g<- g +scale_x_continuous(labels=function(x) x/10e5)
  g<-g+theme_classic()
  g<- g + theme(strip.text.x = element_text(size = 20, face="bold",family="sans"),text=element_text(size=20,family = "sans"), axis.text.x=element_text(angle=90,family = "sans"))
  g<-g+labs(x="Mb", y=l.yl.labs,title=l.main) + theme(axis.ticks.x = element_blank())
  
  return(g)
}


