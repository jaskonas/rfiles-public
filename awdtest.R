setwd("/Users/jda43/Dropbox/Oxbridge info/Spring School/CompTextAnalysis/test")
awd=read.csv("afg.csv")

str(awd)
#start with geocoding

############
# getting maps and  drawing dots
############


library("foreign")
library("ggmap")
library("ggplot2")

map.afg1 <- qmap("afghanistan", source="stamen", zoom=6, maptype="toner", color="bw")
map.afg1+geom_point(data=awd, aes(x=awd$Long, y=awd$Lat), na.rm=T)

#deployment using  density clusters

map.afg2=map.afg1 +
  stat_density2d(data=awd, aes(x=awd$Long, y=awd$Lat), na.rm=T)+
  geom_point(data=awd, aes(x=awd$Long, y=awd$Lat), color="red",alpha=0.03, na.rm=T)
map.afg2
png(filename="afg1.png", width=1000,height =1000,units="px",bg="white")
map.afg2
dev.off()

#testing text stuff
textpar=awd$Summary
library(stm)
library(slam)
library(servr)
library(tm)
library(LDAvis)
CorpusTM=Corpus(VectorSource(textpar))
CorpusTM=tm_map(CorpusTM,removePunctuation)
CorpusTM=tm_map(CorpusTM,removeNumbers)
CorpusTM=tm_map(CorpusTM,tolower)
CorpusTM=tm_map(CorpusTM,removeWords,stopwords('english'))
CorpusTM=tm_map(CorpusTM,stripWhitespace)
CorpusTM=tm_map(CorpusTM,stemDocument)
CorpusTM=tm_map(CorpusTM,PlainTextDocument)
TDM=TermDocumentMatrix(CorpusTM)
dim(TDM)
findFreqTerms(TDM,lowfreq=1,highfreq=1)

TDMS=removeSparseTerms(TDM,0.99)
dim(TDMS)
out= readCorpus(TDMS, type="slam")
names = paste(awd$Title)
out=prepDocuments(out$documents, out$vocab, names)
summary(out)
TMResult=stm(out$documents, out$vocab, K=10, max.em.its = 20, data = out$meta)
save(TMResult, file="TMResult.RData")
load("TMResult.RData")

TMResult2=stm(out$documents, out$vocab, K=6, max.em.its = 20, data = out$meta)
save(TMResult2, file="TMResult2.RData")
load("TMResult2.RData")

plot.STM(TMResult2, type="summary", xlim=c(0,5))
mod.out.corr=topicCorr(TMResult2)
library(igraph)
plot.topicCorr(mod.out.corr)

toLDAvis(mod=TMResult2, out$documents)
