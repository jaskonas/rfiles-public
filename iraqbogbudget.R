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
spend=read.csv("oifbogbudget.csv")
spend$Date = as.Date(paste(1, "Jan", spend$Year), format='%d %B %Y')
spend$per= spend$Budget / spend$Troops * 1000000000

dpers= ggplot(data=spend) + geom_line(aes(x=spend$Date, y=spend$Troops)) + xlab("Year") + geom_line(aes(x=spend$Date, y=spend$Budget*1000), linetype=2)+
  # geom_vline(xintercept = as.Date('1/1/2003',format='%m/%d/%Y'), linetype = 2) 
  annotate("text", x= as.Date('10/1/2004',format='%m/%d/%Y'), y=155000,label = 'Troop Numbers',angle=0, size=4) +
  annotate("text", x= as.Date('7/1/2004',format='%m/%d/%Y'), y=44000,label = 'Budget',angle=0, size=4) +
  theme_bw() + xlim(as.Date(c('1/1/2003','1/1/2011'),format='%m/%d/%Y')) + ylim(40000,160000)+
  scale_y_continuous("Average Number of US Servicemembers in Iraq", sec.axis = sec_axis(~./1000, name = "OIF Budget in Billions"))
dpers
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/oifbogbudget.png", width = 7, height = 5, dpi = 300, units = "in", device='png')


dpers2= ggplot(data=spend) + geom_line(aes(x=spend$Date, y=spend$per)) + ylab("Dollars Spent per Servicemember") + xlab("Year") + 
# geom_vline(xintercept = as.Date('1/1/2003',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=200000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw() + xlim(as.Date(c('1/1/2003','1/1/2011'),format='%m/%d/%Y'))
dpers2
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/oifbogbudget2.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

###Fix with Army Share of Spending
oifspend = ggplot(data=spend) + geom_line(aes(x=spend$Date, y=spend$Budget), linetype=1) + ylab("OIF Budget in Billions (2009 Constant Dollars)") + xlab("Year") +
  # geom_vline(xintercept = as.Date('1/1/2003',format='%m/%d/%Y'), linetype = 2) 
  geom_vline(xintercept = as.Date('2/10/2007',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('12/1/2006',format='%m/%d/%Y'), y=70,label = 'Petraeus Takes Command',angle=90, size=5)+
  theme_bw() + xlim(as.Date(c('1/1/2004','1/1/2012'),format='%m/%d/%Y')) +
  scale_y_continuous(labels=comma)
oifspend
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/budgetnumbers.png", width = 7, height = 4.5, dpi = 300, units = "in", device='png')

###Overall DOD (non OIF only) budget numbers

armyspend=read.csv("dod2001budget.csv")
armyspend$Date = as.Date(paste(1, "Jan", armyspend$Year), format='%d %B %Y')
armyspend$ArmyBaseC = armyspend$ArmyBase / armyspend$Deflator * 100
armyspend$ArmyOCOC = armyspend$ArmyOCO / armyspend$Deflator * 100
armyspend$ArmyOtherC = armyspend$ArmyOther / armyspend$Deflator * 100
armyspend$ArmyTotalC = armyspend$ArmyBaseC + armyspend$ArmyOCOC + armyspend$ArmyOtherC

armyspend2 = ggplot(data=armyspend) + geom_line(aes(x=armyspend$Date, y=armyspend$ArmyTotalC/1000), linetype=1) + geom_line(aes(x=armyspend$Date, y=armyspend$ArmyOCOC/1000), linetype=2) + ylab("Army Budget in Billions (2018 Constant Dollars)") + xlab("Year") +
  # geom_vline(xintercept = as.Date('1/1/2003',format='%m/%d/%Y'), linetype = 2) 
  geom_vline(xintercept = as.Date('2/10/2007',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('12/1/2006',format='%m/%d/%Y'), y=107,label = 'Petraeus Takes Command',angle=90, size=5)+
  annotate("text", x= as.Date('6/1/2010',format='%m/%d/%Y'), y=245,label = 'Total Allocation',angle=0, size=5)+
  annotate("text", x= as.Date('6/1/2010',format='%m/%d/%Y'), y=130,label = 'OCO Allocation',angle=0, size=5)+
  theme_bw() + xlim(as.Date(c('1/1/2004','1/1/2012'),format='%m/%d/%Y')) +
  scale_y_continuous(labels=comma)

armyspend2
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/ocobudgetnumbers.png", width = 7, height = 4.5, dpi = 300, units = "in", device='png')

armydrawdown = ggplot(data=armyspend) + geom_line(aes(x=armyspend$Date, y=armyspend$ArmyTotalC/1000), linetype=1) + geom_line(aes(x=armyspend$Date, y=armyspend$ArmyOCOC/1000), linetype=2) + ylab("Army Budget in Billions (2018 Constant Dollars)") + xlab("Year") +
  # geom_vline(xintercept = as.Date('1/1/2003',format='%m/%d/%Y'), linetype = 2) 
  # geom_vline(xintercept = as.Date('12/30/2011',format='%m/%d/%Y'), linetype = 2) +
  annotate("text", x= as.Date('6/1/2010',format='%m/%d/%Y'), y=245,label = 'Total Allocation',angle=0, size=5)+
  annotate("text", x= as.Date('6/1/2010',format='%m/%d/%Y'), y=130,label = 'OCO Allocation',angle=0, size=5)+
  theme_bw() + xlim(as.Date(c('1/1/2001','1/1/2018'),format='%m/%d/%Y')) +
  scale_y_continuous(labels=comma)

armydrawdown
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/postiraqdrawdown.png", width = 7, height = 4.5, dpi = 300, units = "in", device='png')
