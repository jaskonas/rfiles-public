setwd("/Users/jda43/Dropbox/Oxbridge info/DPhil/Data")
library("foreign")
library(rgeos)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggmap)

khes=read.dta("hes/Kalyvas_Kocher/KK_replication_data_h.dta")
khes2=read.dta("hes/Kalyvas_Kocher/KK_replication_data_v.dta")
#Preview the Data
str(khes)
