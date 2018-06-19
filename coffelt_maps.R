library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(ggmap)
library(maps)
setwd("~/Dropbox/Oxbridge info/DPhil/Data")
coffelt=read.csv("coffelt4.csv")
coffelt2000=coffelt[1:2000,] #comment in for testing

# Histogram of field grade officers (Army and Marines) and above
coffeltfgo= subset(coffelt, (SV=='A' | SV=='M') & (RNK=='MAJ' | RNK=='LTC' | RNK=='COL' | RNK=='BGEN' | RNK=='MGEN'))
coffeltfgo$DOB=as.Date(coffeltfgo$DOB, "%m/%d/%Y")
coffeltfgo$CDATE=as.Date(coffeltfgo$CDATE, "%m/%d/%Y") #need to add in code to pre-process this for people that don't have the exact dates.
coffeltfgo$DDATE=as.Date(coffeltfgo$DDATE, "%m/%d/%Y")
coffeltfgo$TDATE=as.Date(coffeltfgo$TDATE, "%m/%d/%Y")
fgohist=ggplot(coffeltfgo, aes(CDATE)) + geom_histogram(aes(fill = RNK),binwidth = 30)

#Death chart by Vietnam Province

#Pull US Geo Data
# usa <- map_data("state")
# coffelt2000 <- cbind(geocode(as.character(coffelt2000$ST)), coffelt2000)
# cbind()

#Pull in Vietnam Shapefile and Correct Erratta

#Merge MR/CTZ and Province Code into Shapefile


