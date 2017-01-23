library(ggplot2)
setwd("/Users/jda43/Downloads")
d = read.csv("trumpumargin.csv")
summary(d)
d[,4]=d[,2]-d[,3]
colnames(d)[4]='Difference'
d=data.frame(d)
d$Difference
d2=d[order(d$Difference),]
d2
str(d2)
ggplot(data=d2, aes(x=reorder(University, -Difference),y=Difference,)) +
         geom_bar(stat = "identity", aes(fill=Difference), width = .8) + 
  scale_fill_gradient2(low = "#2200E5", mid = "#FFFFFF", high = "#CB152D", midpoint = 0, guide = "colourbar",limits=c(-17,56))+
          coord_flip()+
         ggtitle("The University Bubble Quantified")+
  xlab("Flagship Universities") + ylab("Number of points by which \nflagship university counties' support\n for Clinton outpaced state average")+
theme(plot.title=element_text(size = 12),axis.title = element_text(size = 10),axis.text.y=element_text(size=8),legend.position="none")