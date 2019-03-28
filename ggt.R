library(ggplot2)
library(ggthemes)
library(ggtern)
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
# library(lattice)
# library(gridExtra)
# library(grid)

setwd("~/Dropbox/Governance Triangle Coding/PlayingAroundData")

#Initial Data Cleaning
gttri=read.csv("gt_tri.csv")
gttri$YEARDATE = as.Date(paste(1, "Jun", gttri$YEAR), format='%d %B %Y')
gttri$YEARDN = as.Date.numeric(gttri$YEAR, origin="1976-01-01")
# gttri$YEARLEVEL= 
gttri$STATE = as.numeric(as.character(gttri$STATE))
gttri$FIRM = as.numeric(as.character(gttri$FIRM))
gttri$NGO = as.numeric(as.character(gttri$NGO))

gttri=gttri[complete.cases(gttri),]

#Visualization
gttri_gg = ggtern(data=gttri, aes(STATE,FIRM,NGO, color=gttri$YEARDATE))+
  geom_point(alpha=0.6)+
  theme(legend.title=element_blank())
  # scale_colour_distiller(palette = "RdPu")
gttri_gg
ggsave("ggt_graphic1.png", width = 8, height = 5, dpi = 300, units = "in", device='png')
