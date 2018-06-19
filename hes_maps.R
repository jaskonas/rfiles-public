setwd("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data")
hes=read.csv("hes/HES_gazateer_v1.csv")

str(hes)
#start with geocoding

############
# getting maps and  drawing dots
############
library("foreign")
library(rgeos)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggmap)


svn= readOGR("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data/Maps/SE_ASIA_PROVINCES_SV_NV_KH_LA", "SE_ASIA_PROVINCES_SV_NV_KH_LA")
svo=subset(svn, COUNTRY=='SV')
##do it in ggplot
svo$id = rownames(as.data.frame(svo))
svo.pts <- fortify(svo, region='id') #this only has the coordinates
svo.df <- merge(svo.pts, svo, by="id", type='left')
#map1
map.viet1 <- ggplot(svo.df, aes(long,lat, group=group)) + # the data
  # make polygons
  geom_polygon(aes(x = long, y = lat, group = group), color = "black") +
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()+
  guides(fill=FALSE)

#Heatmap
map.viet2=map.viet1 +
  stat_density2d(data=hes, aes(x=hes$LONG, y=hes$LAT), na.rm=T)+
  geom_point(data=hes, aes(x=hes$LONG, y=hes$LAT, size=hes$TOTALPOP/2), color="red", alpha=0.01, show.legend = FALSE, na.rm=T)+ theme(legend.position="none")

map.viet2
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/hes1.png", width=1200,height =1200,units="px",bg="white")
map.viet2
dev.off()
#compare
map.viet3=map.viet1 +
  stat_density2d(data=hes, aes(x=hes$LONG, y=hes$LAT), na.rm=T)+
  geom_point(data=hes, aes(x=hes$LONG, y=hes$LAT), color="red",alpha=0.03, na.rm=T)
map.viet3
#File used for demographic triptych in Chp 3
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/hes1.png", width=1200,height =1200,units="px",bg="white")
map.viet3
dev.off()

