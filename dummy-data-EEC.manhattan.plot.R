source("EEC.manhattan.plot.R")

set.seed(123)
test.data<-data.frame("chr"=rep("3R",100),"pos"=c(1:100),"value"=as.numeric(sample(x=c(1:500),size = 100,replace = T)))
test.data
str(test.data)
get.plot.highlight(test.data)
get.plot.highlight(l.main = "My manhattan plot",data = test.data)
get.plot.highlight(l.main = "My manhattan plot",data = test.data,l.y.max = 300)
get.plot.highlight(l.main = "My manhattan plot",data = test.data,l.y.min = 50, l.y.max = 500)
get.plot.highlight(l.main = "My manhattan plot",data = test.data,l.yl.labs = "Something different than -log10(p)")
get.plot.highlight(data=test.data,highlight=test.data)
test.highlight<-test.data[sample(x = c(1:nrow(test.data)),size =10,replace = F),]
get.plot.highlight(data = test.data,highlight = test.highlight)
test.highlight$col<-rainbow(10)
get.plot.highlight(data=test.data,highlight = test.highlight,highlight.col = test.highlight$col)
