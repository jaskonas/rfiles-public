library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(ggmap)
library(maps)
library(magick)
library(rgeos)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(lattice)
library(gridExtra)
library(grid)
library(plotly)

setwd("~/Dropbox/Oxbridge info/DPhil/Data")
coffelt=read.csv("coffelt4.csv")
coffelt2000=coffelt[sample(nrow(coffelt),2000),] #comment in for testing

# Histogram of field grade officers (Army and Marines) and above
coffeltfgo= subset(coffelt, (SV=='A' | SV=='M') & (RNK=='MAJ' | RNK=='LTC' | RNK=='COL' | RNK=='BGEN' | RNK=='MGEN'))
coffeltfgo$DOB=as.Date(coffeltfgo$DOB, "%m/%d/%Y")
coffeltfgo$CDATE=as.Date(coffeltfgo$CDATE, "%m/%d/%Y") #need to add in code to pre-process this for people that don't have the exact dates.
coffeltfgo$DDATE=as.Date(coffeltfgo$DDATE, "%m/%d/%Y")
coffeltfgo$TDATE=as.Date(coffeltfgo$TDATE, "%m/%d/%Y")
fgohist=ggplot(coffeltfgo, aes(CDATE)) + geom_histogram(aes(fill = RNK),binwidth = 60)

#map against totals.

#Officer deaths as percentage of all deaths over time (need to isolate hostile deaths or whatever)

#Death chart by Vietnam Province
svn= readOGR("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data/Maps/SE_ASIA_PROVINCES_SV_NV_KH_LA", "SE_ASIA_PROVINCES_SV_NV_KH_LA")
svo=subset(svn, COUNTRY=='SV')
corps=read.csv("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/corps.csv")
combined <- sort(union(levels(corps$Province), levels(svo$Province)))

svo@data = left_join(mutate(svo@data, Province = factor(Province, levels=combined)), unique(mutate(corps, Province = factor(Province, levels = combined))))
##do it in ggplot
svo$id = rownames(as.data.frame(svo))
svo.pts <- fortify(svo, region='id') #this only has the coordinates
svo.df <- merge(svo.pts, svo, by="id", type='left')
svo.df$Corps = as.factor(svo.df$Corps)
svo.df$Coffelt.Code = as.factor(svo.df$Coffelt.Code)
#base
svbase= ggplot(svo.df, aes(long,lat, group=group)) + # the data
  #brew
  # make polygons
  geom_polygon(aes(x = long, y = lat, group = group), color = "black", fill='white') +
  
  theme(line = element_blank(),  # remove the background, tickmarks, etc
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank()) +
  coord_equal()

#province count casualties
by_province <- group_by(coffelt, PR)
provincecount= summarize (by_province, count = n())
colnames(provincecount)[1]="Coffelt.Code"
combined2 <- sort(union(levels(svo.df$Coffelt.Code), levels(provincecount$Coffelt.Code)))


svo@data = left_join(mutate(svo@data, Coffelt.Code = factor(Coffelt.Code, levels=combined2)), unique(mutate(provincecount, Coffelt.Code = factor(Coffelt.Code, levels = combined2))))
promap <- inner_join(svo.df, provincecount, by = "Coffelt.Code")
promap$Coffelt.Code=as.factor(promap$Coffelt.Code)
#mapcasualties
casmap = svbase + geom_polygon(data = promap, aes(fill = count), color = "white")
# casmapcol = casmap + scale_fill_gradient(low="blue", high = "red",trans='log10')
casmapcol = casmap + scale_fill_gradientn(colours = rev(rainbow(9)),
                     breaks = c(10, 100, 250, 500, 1000, 2000, 4000, 8000),
                     trans = "log10") + theme(legend.position = c(0.15, 0.75)) + labs(fill='Number of\nUS Deaths')
casmapcol
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/casmapprov.png", width = 3, height = 5, dpi = 300, units = "in", device='png')
#plotly post
c1 = ggplotly(casmapcol)
# api_create(c1, filename = "VN US Casualty Map",sharing = "private") #Need to fix subscription


#HES overlay
hes=read.csv("hes/HES_gazateer_v1.csv")
hesoverlay= casmapcol+
  stat_density2d(data=hes, inherit.aes = FALSE, aes(x=hes$LONG, y=hes$LAT), na.rm=T)+
  geom_point(data=hes, inherit.aes = FALSE, aes(x=hes$LONG, y=hes$LAT), color="red",alpha=0.03, na.rm=T)

hesoverlay
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/casmapcol_hesoverlay1.png", width=1200,height =1200,units="px",bg="white")
hesoverlay
dev.off()

#Bar Chart by Province Coffelt
combined3 <- sort(union(levels(corps$Coffelt.Code), levels(provincecount$Coffelt.Code)))
provincecount2 = inner_join(mutate(provincecount, Coffelt.Code = factor(Coffelt.Code, levels=combined3)), unique(mutate(corps, Coffelt.Code = factor(Coffelt.Code, levels = combined3))), by = "Coffelt.Code")
#might need to refactor b/c corps has other country provinces

casbarchartprov= ggplot(data=provincecount2, aes(Province,count))+
  geom_col(aes(reorder(Province, count), fill=count), color="grey20")
casbarchartprov
casbarchartprov= casbarchartprov + scale_fill_distiller(palette = "Spectral") + ylab("Number of Deaths") +theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1,size=8))

casbarchartprov
png(filename="/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Images/casbarchartprov.png", width=1200,height =1200,units="px",bg="white")
casbarchartprov
dev.off()

#putcasbarchart together with casmapcol

#NEED TO REDO CASBARCHART WITH GEOGRAPHICAL DISPLAY PROVINCES ONLY Or do a version with legend a la stack overflow saved in Pocket
casbarchartblank= casbarchartprov+theme_nothing() +  scale_fill_gradientn(colours = rev(rainbow(9)), breaks = c(10, 100, 250, 500, 1000, 2000, 4000, 8000))
grid.arrange(casmapcol,casbarchartblank, ncol=2, top = textGrob("Title",gp=gpar(fontsize=20,font=3)))
#Pull US Geo Data
# usa <- map_data("state")
# coffelt2000 <- cbind(geocode(as.character(coffelt2000$ST)), coffelt2000)
# cbind()

#Pull in Vietnam Shapefile and Correct Erratta

#Merge MR/CTZ and Province Code into Shapefile


