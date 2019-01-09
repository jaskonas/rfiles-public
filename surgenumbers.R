library(ggplot2)
library(ggthemes)
# library(scales)
# library(grid)
# library(ggmap)
# library(maps)
# library(magick)
# # library(rgeos)
# library(rgdal)
# library(dplyr)
# library(RColorBrewer)
# library(lattice)
# library(gridExtra)
# library(grid)
# library(plotly)

setwd("~/Dropbox/Oxbridge info/DPhil/Data")
bog=read.csv("bog_iraq.csv") #
bog$Month=as.character(bog$Month)
bog$Month = paste('01', bog$Month)
bog$Month= as.Date(bog$Month, format="%d %B %y")
bog$iraqBOG=as.integer(gsub(",", "", bog$iraqBOG))
bog$OIF=as.integer(gsub(",", "", bog$OIF))
bog$OIFlessBOG=as.integer(gsub(",", "", bog$OIFlessBOG))
bog2=ggplot(data=bog) + geom_line(aes(x=Month, y=iraqBOG))+ ylab("Troops on the Ground in Iraq") + # Set axis labels
  geom_vline(xintercept = as.Date('7/1/2007',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('4/1/2007',format='%m/%d/%Y'), y=70000,label = 'Surge Troops Arrive',angle=90, size=5) + theme_bw() + xlim(as.Date(c('1/1/2004','1/1/2012'),format='%m/%d/%Y'))
bog2
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/surgenumbers_2008.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

bogfull=read.csv("bog_iraq2.csv") #
bogfull$Month=as.character(bogfull$Month)
bogfull$Month = paste('1/', bogfull$Month, sep = "")
bogfull$Month= as.Date(bogfull$Month, format="%d/%m/%y")
bogfull$iraqBOG=as.integer(gsub(",", "", bogfull$iraqBOG))

bogfull2=ggplot(data=bogfull) + geom_line(aes(x=Month, y=iraqBOG))+ ylab("Troops on the Ground in Iraq") + # Set axis labels
  geom_vline(xintercept = as.Date('7/1/2007',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('4/1/2007',format='%m/%d/%Y'), y=70000,label = 'Surge Troops Arrive',angle=90, size=5) + theme_bw() + xlim(as.Date(c('1/1/2004','1/1/2012'),format='%m/%d/%Y'))
bogfull2
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/surgenumbers2.png", width = 7, height = 5, dpi = 300, units = "in", device='png')

bogfullfin=ggplot(data=bogfull) + geom_line(aes(x=Month, y=iraqBOG/1000))+ ylab("Troops on the Ground in Iraq in Thousands") + # Set axis labels
  geom_vline(xintercept = as.Date('7/1/2007',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('4/1/2007',format='%m/%d/%Y'), y=70,label = 'Surge Troops Arrive',angle=90, size=5) + theme_bw() + xlim(as.Date(c('1/1/2004','1/1/2012'),format='%m/%d/%Y'))
bogfullfin
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/surgenumbers.png", width = 7, height = 4.5, dpi = 300, units = "in", device='png')
