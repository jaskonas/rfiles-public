library(ggplot2)
library(ggthemes)
# library(scales)
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
rob=read.csv("iraqrobots.csv")
rob$Date = as.Date(paste(1, "Jan", rob$Year), format='%d %B %Y')

robs= ggplot(data=rob) + geom_line(aes(x=rob$Date, y=rob$RoboticSystems)) + ylab("Robotic Systems Deployed")+ xlab("Year") +
  theme_bw() + xlim(as.Date(c('1/1/2003','1/1/2008'),format='%m/%d/%Y'))
robs
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/iraqrobots.png", width = 7, height = 5, dpi = 300, units = "in", device='png')