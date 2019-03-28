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

setwd("~/Dropbox/CUA_CSS/Research/InflationCOIN")

#GovEmployees
dacy_govtemp=read.csv("Dacy_47_Table3_3_GovtEmpThousands.csv")
dacy_govtemp$Year = as.Date(paste(1, "Jun", dacy_govtemp$Year), format='%d %B %Y')
dacy_govtemp = gather(dacy_govtemp, Type, Number, -Year)

dacy_govtempgg=ggplot(data=dacy_govtemp) + geom_line(aes(x=dacy_govtemp$Year, y=Number, linetype=Type))+ ylab("GVN Employees (000s)") + xlab("Year") +# Set axis labels
  #geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw() #+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
dacy_govtempgg
ggsave("/Users/jda43/Dropbox/CUA_CSS/Writing/2018/inflationcoin/Graphics/vngovtemp.png", width = 5, height = 4, dpi = 300, units = "in", device='png')

#RealNDP #FIXXX
dacy_sectoralNDP=read.csv("Dacy_60_Table3_6_Sectoral_RealNDPBil.csv")
dacy_sectoralNDP$Year = as.Date(paste(1, "Jun", dacy_sectoralNDP$Year), format='%d %B %Y')
dacy_sectoralNDP = gather(dacy_sectoralNDP, Type, Number, -Year)

#fix
dacy_sectoralNDPggtrace=ggplot(data=subset(dacy_sectoralNDP, Type %in% c("Gov_Per", "Serv_Per"))) + geom_line(aes(x=Year, y=Number, linetype=Type))+ ylab("Percent National Income by Sector") + xlab("Year")+#+# Set axis labels
  #geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw() +#+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
  scale_linetype_discrete(name="", breaks=c("Gov_Per","Serv_Per"), labels=c("Government","Services"))


  # geom_line(data=subset(dacy_sectoralNDP, Type %in% c("Gov_Per")), aes(x=Year, y=Number), color="red")+
  # geom_line(data=subset(dacy_sectoralNDP, Type %in% c("Serv_Per")), aes(x=Year, y=Number), color="blue")
dacy_sectoralNDPggtrace
ggsave("/Users/jda43/Dropbox/CUA_CSS/Writing/2018/inflationcoin/Graphics/sectoralNDPtrace.png", width = 5, height = 4, dpi = 300, units = "in", device='png')

#Trade Numbers

dacy_Trade=read.csv("Dacy_83_Table_4_3_TradeDefDolM.csv")
dacy_Trade$Year = as.Date(paste(1, "Jun", dacy_Trade$Year), format='%d %B %Y')
# dacy_Trade = gather(dacy_Trade, Type, Number, -Year)

dacy_Tradegg=ggplot(data=dacy_Trade) + geom_line(aes(x=dacy_Trade$Year, y=ImportPriceIndex))+ ylab("Import Price Index") + xlab("Year") +# Set axis labels
  #geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw() #+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
dacy_Tradegg
ggsave("/Users/jda43/Dropbox/CUA_CSS/Writing/2018/inflationcoin/Graphics/vnimportpriceindex.png", width = 5, height = 4, dpi = 300, units = "in", device='png')

#Saigonwages
dacy_saigonwage=read.csv("Dacy_118_Tab5_2_RealWageSaigonKgRiceIndex.csv")
dacy_saigonwage$Year = as.Date(paste(1, "Jun", dacy_saigonwage$Year), format='%d %B %Y')
dacy_saigonwage = gather(dacy_saigonwage, Type, Number, -Year)

dacy_saigonwagegg=ggplot(data=dacy_saigonwage) + geom_line(aes(x=dacy_saigonwage$Year, y=Number, linetype=Type))+ ylab("Skilled and Unskilled Real Wages, Saigon") + xlab("Year") +# Set axis labels
  #geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw()+ #+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
  scale_linetype_discrete(name="", breaks=c("SkAll","SkRice","UnsAll","UnsRice"), labels=c("Skilled, Overall","Skilled, Rice", "Unskilled Overall", "Unskilled Rice"))
dacy_saigonwagegg
ggsave("/Users/jda43/Dropbox/CUA_CSS/Writing/2018/inflationcoin/Graphics/dacy_saigonwage.png", width = 5, height = 4, dpi = 300, units = "in", device='png')



#Price Baskets
dacy_VNpriceindex=read.csv("Dacy_134_135_Tab7_1_VNpriceindex.csv")
dacy_VNpriceindex$Year = as.Date(paste(1, "Jun", dacy_VNpriceindex$Year), format='%d %B %Y')
dacy_VNpriceindex = gather(dacy_VNpriceindex, Type, Number, -Year)

dacy_VNpriceindexgg=ggplot(data=dacy_VNpriceindex) + geom_line(aes(x=dacy_VNpriceindex$Year, y=Number, linetype=Type))+ ylab("Price Indices (1962 = 100)") + xlab("Year")+# Set axis labels
  #geom_vline(xintercept = as.Date('1/1/1973',format='%m/%d/%Y'), linetype = 2) + annotate("text", x= as.Date('7/1/1973',format='%m/%d/%Y'), y=1000000,label = 'Last Draftee Enters Army',angle=90, size=4) + 
  theme_bw()+ #+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
  scale_linetype_discrete(name="Index", breaks=c("MidClassIndex","WholesaleIndex","WorkingclassIndex"), labels=c("Middle Class","Wholesale","Working Class"))
dacy_VNpriceindexgg
ggsave("/Users/jda43/Dropbox/CUA_CSS/Writing/2018/inflationcoin/Graphics/dacy_VNpriceindex.png", width = 10, height = 5, dpi = 300, units = "in", device='png')

#aid levels

#gvn budget and tax

#monetary velocity 
