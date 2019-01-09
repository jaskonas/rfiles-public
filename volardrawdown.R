library(ggplot2)
library(ggthemes)
library(scales)
# library(grid)
# library(ggmap)
# library(maps)
# library(magick)
# # library(rgeos)
# library(rgdal)
library(dplyr)
# library(RColorBrewer)
# library(lattice)
# library(gridExtra)
# library(grid)
# library(plotly)

setwd("~/Dropbox/Oxbridge info/DPhil/Data")
dod=read.csv("dodpersonnelhistoricdata.csv")
dod$Date = as.Date(paste(1, "Jun", dod$Year), format='%d %B %Y')
dod$ArmyTotal = dod$ArmyEnlisted + dod$ArmyOfficers
dod$totaloff=dod$ArmyOfficers+dod$NavyOfficers+dod$MarineOfficers+dod$AirForceOfficers
dod$totalen= dod$ArmyEnlisted + dod$NavyEnlisted + dod$MarineEnlisted + dod$AirForceEnlisted
dod$total = dod$totalen + dod$totaloff
#graphs
dodp=ggplot(data=dod) + geom_line(aes(x=dod$Date, y=ArmyEnlisted))+ ylab("Army Enlisted") + xlab("Year") +# Set axis labels
  geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + theme_bw() + ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
dodp
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/volardrawdownenlisted.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

dodp2 = ggplot(data=dod) + geom_line(aes(x=dod$Date, y=ArmyEnlisted), linetype=2)+ ylab("Army Manpower") + xlab("Year") + 
  geom_line(aes(x=dod$Date, y=ArmyTotal))+# Set axis labels
  geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('10/1/1973',format='%m/%d/%Y'), y=1250000,label = 'Last Draftee Enters Army',angle=90, size=4) + theme_bw() + ylim(500000, 1600000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))+
  annotate("text", x= as.Date('7/1/1983',format='%m/%d/%Y'), y=815000,label = 'Total', size=4)+
  annotate("text", x= as.Date('7/1/1983',format='%m/%d/%Y'), y=625000,label = 'Enlisted', size=4)
dodp2
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/volardrawdown.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

spend=read.csv("defspendpostnam.csv")
spend$Date = as.Date(paste(1, "Jun", spend$Year), format='%d %B %Y')
dodlim= inner_join(dod,spend,by='Date')
dodlim$spendper = dodlim$DefSpend *1000000000 / dodlim$total

dpers= ggplot(data=dodlim) + geom_line(aes(x=dodlim$Date, y=spendper)) + ylab("Dollars Spent per Servicemember") + xlab("Year") + 
geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=200000,label = 'Last Draftee Enters Army',angle=90, size=4) + theme_bw() + xlim(as.Date(c('1/1/1962','1/1/1991'),format='%m/%d/%Y'))
dpers
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/dollarsperserv1962-1990.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

###Fix with Army Share of Spending

#postIraq
dodp3 = ggplot(data=dod) + geom_line(aes(x=dod$Date, y=ArmyEnlisted), linetype=2)+ ylab("Army Manpower") + xlab("Year") + 
  geom_line(aes(x=dod$Date, y=ArmyTotal))+  geom_vline(xintercept = as.Date('1/1/2011',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('10/1/2011',format='%m/%d/%Y'), y=355000,label = 'US Exits Iraq',angle=90, size=4)+# Set axis labels
  theme_bw() + xlim(as.Date(c('1/1/2001','1/1/2019'),format='%m/%d/%Y'))+
  annotate("text", x= as.Date('7/1/2005',format='%m/%d/%Y'), y=525000,label = 'Total', size=4)+
  annotate("text", x= as.Date('7/1/2005',format='%m/%d/%Y'), y=385000,label = 'Enlisted', size=4) + scale_y_continuous(limits= c(300000,600000),labels=comma)
dodp3
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/postiraqmanpower.png", width = 7, height = 5, dpi = 300, units = "in", device='png')
