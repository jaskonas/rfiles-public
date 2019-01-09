library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(ggmap)
# library(maps)
library(magick)
library(rgeos)
library(rgdal)
library(dplyr)
library(RColorBrewer)
# library(lattice)
# library(gridExtra)
# library(grid)
# library(plotly)

setwd("~/Dropbox/Oxbridge info/DPhil/Data")
cas_names=read.csv("oifcas.csv")
cas_names$DDATE=as.Date(cas_names$DDATE, "%Y/%m/%d")
wholehist=ggplot(cas_names, aes(DDATE)) + geom_histogram(binwidth = 60) + 
  xlim(as.Date("2003-01-01"),as.Date("2011-01-01"))+
  theme_bw()
wholehist
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/iraqcashist.png", width = 5, height = 5, dpi = 300, units = "in", device='png')
casiraq=cas_names[cas_names$CasCountry=='IRAQ',]
casiraq$CasCity= factor(casiraq$CasCity)
iraqCities=tibble(levels(casiraq$CasCity))
colnames(iraqCities) = 'cities'
latlong = geocode(paste(iraqCities$cities, 'IRAQ'))

##IED attacks
cas_ied=read.csv("icasualtiesoif_ied.csv")
cas_ied$Date = as.character(cas_ied$Date)
cas_ied$Date=as.Date(cas_ied$Date, "%m/%d/%y")
iedhist=ggplot(cas_ied, aes(Date)) + geom_histogram(binwidth = 60) +
 xlim(as.Date("2003-01-01"),as.Date("2012-01-01"))+
  theme_bw() + ylab("Deaths")
iedhist
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/iraqcashist_ied.png", width = 5, height = 5, dpi = 300, units = "in", device='png')


###Maps
iraq= readOGR("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data/Maps/Iraq Districts", "iraq_districts")
iraq$id = rownames(as.data.frame(iraq))
iraq.pts <- fortify(iraq, region='id') #this only has the coordinates
iraq.df <- merge(iraq.pts, iraq, by="id", type='left')

#plotitout
iraqcasmap=ggplot(iraq.df, aes(long,lat, group=group)) + # the data
  # make polygons
  geom_polygon(aes(x = long, y = lat, group = group), fill= 'white', color = "grey20")

#add data
iraqcasmap=iraqcasmap+
  stat_density2d(data=hes, aes(x=hes$LONG, y=hes$LAT), na.rm=T, inherit.aes = FALSE)+
  geom_point(data=hes, aes(x=hes$LONG, y=hes$LAT, size=hes$TOTALPOP/2), color="red",alpha=0.02, na.rm=T,inherit.aes = FALSE)+
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()+
  guides(fill=FALSE) + theme(legend.position="none")