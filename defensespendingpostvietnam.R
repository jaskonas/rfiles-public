library(ggplot2)
library(ggthemes)
# library(scales)
# library(grid)
# library(ggmap)
# library(maps)
# library(magick)
# # library(rgeos)
# library(rgdal)
# library(dplyr)
# library(RColorBrewer)
# library(lattice)
# library(gridExtra)
# library(grid)
# library(plotly)

setwd("~/Dropbox/Oxbridge info/DPhil/Data")
spend=read.csv("defspendpostnam.csv")
spend$Date = as.Date(paste(1, "Jun", spend$Year), format='%d %B %Y')
#graphs
spendp=ggplot(data=spend) + geom_line(aes(x=spend$Date, y=DefSpend))+ ylab("Spending in Billions of FY2009 Constant Dollars") + xlab("Year") +# Set axis labels
   theme_bw() #+ ylim(500000, 1500000) + xlim(as.Date(c('1/1/1954','1/1/1991'),format='%m/%d/%Y'))
spendp
ggsave("/Users/jda43/Dropbox/Oxbridge info/DPhil/Writing/Latex/thesis_root/img/defensespendingpostvietnam.png", width = 7, height = 5, dpi = 300, units = "in", device='png')
