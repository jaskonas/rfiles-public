#Tell R where you want to work and get the data
setwd("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data")
hes=read.csv("hes/HES_gazateer_v1.csv")
#Preview the Data
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

#Read the shape file (one way to do so)
svn= readOGR("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data/Maps/SE_ASIA_PROVINCES_SV_NV_KH_LA", "SE_ASIA_PROVINCES_SV_NV_KH_LA")
#Limit it to South Vietnam
svo=subset(svn, COUNTRY=='SV')
#Pull my list of which provinces are in which Corps Tactical Zone and about casualties
corps=read.csv("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/corps.csv")
#Add my data on corps and provinces to the shapefile
combined <- sort(union(levels(corps$Province), levels(svo$Province)))

svo@data = left_join(mutate(svo@data, Province = factor(Province, levels=combined)), unique(mutate(corps, Province = factor(Province, levels = combined))))
##do it in ggplot
svo$id = rownames(as.data.frame(svo))
svo.pts <- fortify(svo, region='id') #this only has the coordinates
svo.df <- merge(svo.pts, svo, by="id", type='left')
#Put my data into the right shape
svo.df$Corps = as.factor(svo.df$Corps)
svo.df$Coffelt.Code = as.factor(svo.df$Coffelt.Code)
#base
map.viet1= ggplot(svo.df, aes(long,lat, group=group)) + 
  # make polygons
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill='white') +
  
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()

#Heatmap - adding a heatmap precoded with Lat Long on top of my map
map.viet2=map.viet1 + 
  stat_density2d(data=hes, aes(x=hes$LONG, y=hes$LAT), na.rm=T, inherit.aes = FALSE)+
  geom_point(data=hes, aes(x=hes$LONG, y=hes$LAT, size=hes$TOTALPOP/2), color="red", alpha=0.01, show.legend = FALSE, na.rm=T,inherit.aes = FALSE)+ theme(legend.position="none")

map.viet2
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/hes1.png", width=1200,height =1200,units="px",bg="white")
map.viet2
dev.off()
#Phu Yen Only
py=subset(svo, Province %in% "Phu Yen" )
py.df=subset(svo.df, Province %in% "Phu Yen")
map.py1= ggplot(py.df, aes(long,lat, group=group)) + 
  # make polygons
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill='white') +
  
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()
#convert hes
hescoord <- na.omit(hes[,c("LONG","LAT")])
hes2 = hes[complete.cases(hes[ , c("LAT","LONG")]),]
hesdf <- SpatialPointsDataFrame(coords = hescoord, data = hes2,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#add hes
py_hes=hesdf[py,]
py_hesdf=data.frame(coordinates(py_hes))
map.py2=map.py1 + 
  stat_density2d(data=py_hesdf, aes(x=py_hesdf$LONG, y=py_hesdf$LAT), na.rm=T, inherit.aes = FALSE)+
 geom_point(data=py_hesdf, aes(x=py_hesdf$LONG, y=py_hesdf$LAT), color="red", alpha=0.3, show.legend = FALSE, na.rm=T,inherit.aes = FALSE)+
theme(legend.position="none")
map.py2

map.py2
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/phuyen_hes1.png", width=600,height =600,units="px",bg="white")
map.py2
dev.off()

#isolate village for Rob
library(stringr)
hes3=hes
hes3 = hes3 %>% filter(str_detect(hes3$HAMLETNAME, "^MINH DUC"))
hes3[1,]
#compare
map.viet3=map.viet1 +
  stat_density2d(data=hes, aes(x=hes$LONG, y=hes$LAT), na.rm=T)+
  geom_point(data=hes, aes(x=hes$LONG, y=hes$LAT), color="red",alpha=0.03, na.rm=T)
map.viet3
#File used for demographic triptych in Chp 3
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/hes1.png", width=1200,height =1200,units="px",bg="white")
map.viet3
dev.off()

#Province level #goingtohavetofigurethisoutlater
by_VILID <- group_by(hes, HAMLETID)
Vilad_count= summarize(by_VILID, VilMean=mean(TOTALPOP), prov=mean(PROVINCEID))

provincecount$
colnames(provincecount)[1]="Coffelt.Code"
combined2 <- sort(union(levels(svo.df$Coffelt.Code), levels(provincecount$Coffelt.Code)))

svo@data = left_join(mutate(svo@data, Coffelt.Code = factor(Coffelt.Code, levels=combined2)), unique(mutate(provincecount, Coffelt.Code = factor(Coffelt.Code, levels = combined2))))
promap <- inner_join(svo.df, provincecount, by = "Coffelt.Code")
promap$Coffelt.Code=as.factor(promap$Coffelt.Code)
#mapcasualties
casmap = svbase + geom_polygon(data = promap, aes(fill = count), color = "white")
casmapcol = casmap + scale_fill_gradient(low="blue", high = "red",trans='log10')


