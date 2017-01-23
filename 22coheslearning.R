## Setup for 2x2 research design graph showing zones of agreement (low cohesion-low learning and high-high) and zone of disagreement (high-low)
library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
setwd("~/Dropbox/Oxbridge info/DPhil/Data/Images")
#create graph
cohesion22=ggplot(data = NULL,aes(x=seq(0,1,by=.1), y=seq(0,1,by=.1)))+
  xlab("Learning") + ylab("Cohesion") + # Set axis labels
  ggtitle("Cohesion and Learning: Thinking about Tradeoffs") +
  theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))+
scale_y_continuous(labels=scales::percent, limits=c(0,1)) + scale_x_continuous(labels=scales::percent, limits=c(0,1))
cohesion22=cohesion22+
    geom_segment(aes(x=.35,y=.65,xend=.65,yend=.35),arrow =arrow(length = unit(5,"point"), ends="both",type = "closed"))+
geom_text(label="Axis of Distinguishability",aes(x=.5,y=.5),angle= -45,nudge_x=.05,nudge_y=.05)
cohesion22=cohesion22+geom_label(label="Low Cohesion + Low Learning: \nLow Effectiveness",aes(x=.2,y=.2),size=3)+geom_label(label="High Cohesion + High Learning: \nHigh Effectiveness",aes(x=.8,y=.8),size=3)+geom_label(label="Low Cohesion + High Learning: \nUnknown Effectiveness",aes(x=.8,y=.2),size=3)+geom_label(label="High Cohesion + Low Learning: \nUnknown Effectiveness",aes(x=.2,y=.8),size=3)
ggsave("cohesion22.png", width = 5, height=5)
