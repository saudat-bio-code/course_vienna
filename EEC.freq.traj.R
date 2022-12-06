require(ggplot2)
require(plyr)
require(data.table)

get.af.traj<-function(l.sync,l.cluster,l.cluster.id,n.reps=10,n.gen=7,l.chr=3){
  c.names<-c("chr","pos","ref","riseallele","fallallele","basePops",paste(rep("F",n.reps*n.gen),rep(seq(0,n.gen*10-10,10),each=n.reps),rep(".R",n.reps*n.gen),rep(c(1:10),n.gen),rep(".freq",n.reps*n.gen),sep=""))
  colnames(l.sync)<-c.names
  sub.cluster<-subset(l.cluster,id==l.cluster.id)
  my.data<-subset(l.sync,pos%in%sub.cluster$pos)
  my.data<-as.data.frame(my.data)
  res<-list()
  for(i in c(1:n.reps)){
    grep(paste(".R",i,".freq",sep=""),colnames(my.data),fixed=TRUE)->temp.idx
    temp.data<-my.data[,temp.idx]
    temp.data$chr<-my.data$chr
    temp.data$pos<-my.data$pos
    temp.data$replicate<-rep(i,nrow(temp.data))
    res[[i]]<-temp.data
  }
  res.all<-rbindlist(res,use.names = F)
  melt(res.all,id.vars=c("chr","pos","replicate"))->res
  res$variable<-gsub( "[.].*$", "",res$variable)
  colnames(res)<-c("chr","pos","replicate","generation","frequency")
  res_mean<-ddply(res,.(replicate,generation),summarize,frequency=mean(frequency,na.rm = T))
  res_mean$pos<-0
  p<-ggplot(data=res,aes(x=generation,y=frequency,group=pos))+geom_line(alpha=0.5)+geom_line(data=res_mean,aes(x=generation,y=frequency,group=pos),color=unique(sub.cluster$color),size=1)
  p<-p+facet_wrap("replicate",ncol=5)+theme_minimal()
  p<- p + theme(strip.text.x = element_text(size = 15, face="bold"),text=element_text(size=15))+ ylim(0,1)
  p<-p+labs(x="Generation", y="Frequency",title=paste("Cluster",l.cluster.id,sep=" "))
  p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

