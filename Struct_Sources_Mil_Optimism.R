library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
# library(grid)
# library(ggmap)
# library(maps)
# library(magick)
# # library(rgeos)
# library(rgdal)
library(dplyr)
# library(RColorBrewer)
library(lattice)
library(gridExtra)
library(grid)
# library(plotly)

setwd("~/Dropbox/CUA_CSS/Writing/2019/StructuralSourcesMilOptimism")

VN_trooplevels=read.csv("./Data/VN_trooplevels.csv")
VN_trooplevels$Year = as.Date(paste(1, "Jun", VN_trooplevels$Year), format='%d %B %Y')
VN_trooplevelsgg=ggplot(data=VN_trooplevels) + geom_line(aes(x=VN_trooplevels$Year, y=DODTotal/1000))+ ylab("US Troops in Vietnam (000s)") + xlab("Year") +scale_y_continuous(labels = comma)
VN_trooplevelsgg
ggsave("VNtrooplevels.png", width = 5, height = 4, dpi = 300, units = "in", device='png')

#VN_trooplevels = replace_na(VN_trooplevels, 0)

#1954- notate Operation Passage to Freedom