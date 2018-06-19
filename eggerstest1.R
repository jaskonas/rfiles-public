vec1 = c(1,4,3,6) #one data type in one dimension
vec1==4 #double equal is a truth test. The way this works is that it tests whether EACH element equals 4
vec = c("apple","2 bananas", "pineapple","another fruit")
vec == "apple"

grepl("apple", vec)
grepl("app",vec)

sub("a","A",vec) #what I'm finding, what I am replacing, the data frame
gsub("a","A",vec)##sub only does 1st - gsub does all
vec2=sub("pineapple","pineXXX",vec)
grepl("apple",vec2)
grepl("^apple",vec) ##carrot sign ^ the beginning of the string"the mystery and power of regex"
grepl("^apple", vec)
# inside a regular expression, "^" means "the start of the string of characters."
grepl("^a",vec)
# give us?  1 & 4



# inside a regular expression, "$" means "the end of the string of characters"
# so how do we identify the entries that end with e? 
grepl("e$", vec)

### Some other useful metacharacters 
# "." is the wildcard
# \\d is a digit 
# \\D is non-digit 
# \\s means a space 
# + means one or more 
# * means zero or more 
grepl("\\s", vec)
grepl("\\d", vec)
# [abc] match any one of a, b, or c
grepl("b[ai]n", c("banana", "binary", "bones"))

# another useful thing: "abc|def" means "match abc or def"
grepl("app|ban", vec)
parli=read.csv("https://raw.githubusercontent.com/jaskonas/rfiles-public/master/THC_candidates.csv",stringsAsFactors=F)
parli$oxmatch <- grepl("Oxford",parli$bio)
summary(parli)
head(parli$bio[parli$oxmatch])
parli$bio[grepl("Oxford",parli$bio)]
parli$oxccmatch = grepl("[Oo]xford City",parli$bio)
parli$bio[parli$oxccmatch]
#parli$ms=grepl("^Mrs|^Miss|^Ms",parli$bio)
parli$bio[grepl("^Mrs|^Miss|^Ms",parli$bio)]
###nextlesson
library(tm)
library(SnowballC)
sw=stopwords('english')
#can then modify. Say we are looking for being words, like in Heidegger
#can then stem or lematize
text=c("am","are","is") #stems seperate, but would lematize diff
text2=c("politics","polity","political")
text3=c("communication","communicative",'communicate')
stemDocument(text)
stemDocument(text2)
stemDocument(text3)
#lemmas are better when you can-use koRpus and treetagger

#will need to pre-process metaphors, multiwords, etc.
#there are some multiword lexemes - but no good lemmatization schemes yet
##try out frequent ngrams (frequent combinations of 2 words, 3 words, etc) and collocations
##How to do multiword preprocessing? substitution across the Corpus
##synonyms - wordnetcode.princeton.edu. Download dictionaries and use term filters

#concordances have now been implemented in R
library(quanteda)
#kwic(Corpus,"search term", window=5[where you want it displayed],wholeword=F or T) Can then view and write to a CSV

#word collocations
#Spaces of collocation - surface conoccurence, textual coocurrence, syntactic coocurrence

#implemented in quanteda - collocations function. Gives it to you as observed and expected frequencies

#ngrams- co-occuring words w/in a giving window. Typlical you move one word forward when comparing ngrams. (1 (one word),2,3,4 etc.). might need to do pre-processing to use right
s="Felix struggles finishing this PhD. I am struggling finishing my PhD. I am Andy, struggling with this corpus."
w=strsplit(s," ",fixed=TRUE)[[1L]]
##change 2

#### DAY TWO ###
library(tm)
library(textometry)
library(FactoMineR)

parli=read.csv("https://raw.githubusercontent.com/jaskonas/rfiles-public/master/THC_candidates.csv",stringsAsFactors=F)
parli$oxmatch <- grepl("Oxford",parli$bio)
summary(parli)

r=data("robespierre")
specificities(robespierre,types=NULL,parts=NULL)

#text objects
#create text object
textpar=parli$bio
CorpusTM=Corpus(VectorSource(textpar))
CorpusTM=tm_map(CorpusTM,removePunctuation)
CorpusTM=tm_map(CorpusTM,removeNumbers)
CorpusTM=tm_map(CorpusTM,tolower)
CorpusTM=tm_map(CorpusTM,removeWords,stopwords('english'))
CorpusTM=tm_map(CorpusTM,stripWhitespace)
CorpusTM=tm_map(CorpusTM,stemDocument)
CorpusTM=tm_map(CorpusTM,PlainTextDocument)
TDM=TermDocumentMatrix(CorpusTM)

#playing around
terms=c("Oxford","Cambridge")
dim(TDM)
inspect(TDM[15:50,1])
TDMS =removeSparseTerms(TDM,0.99)
inspect(TDMS[15:30,1])
dim(TDMS)
names=paste(parli$constituency.name,parli$sname)
colnames(TDMS)=names
TDMSMat=as.matrix(TDMS)
spe=specificities(TDMSMat)
plot(spe[7,]) #Africa

subbio=which(spe[7,]>3) #get hgh up ones


plot(spe[23,]) #artillery 

plot(subbio[23,])
##FIGURE OUT SUBBIO


library(FactoMineR)
TDMSMat[TDMSMat==0]=0.00000000001
CA(TDMSMat)
res.ca=CA(TDMSMat[,(grep("1950",parli$date))], ncp=2)
res.ca=CA(TDMSMat[,(grep("[Bb]attle",parli$bio))], ncp=2)
plot(res.ca, invisible = "col")

#SCALING DATA
parli=read.csv("https://raw.githubusercontent.com/jaskonas/rfiles-public/master/THC_candidates.csv",stringsAsFactors=F)
parli$oxmatch <- grepl("Oxford",parli$bio)
summary(parli)
head(parli$bio[parli$oxmatch])
parli$bio[grepl("Oxford",parli$bio)]
parli$oxccmatch = grepl("[Oo]xford City",parli$bio)
table(parli$party)
sort(table(parli$party))
parli$party[parli$party=="L."]="L"
parli$party[parli$party=="lab"]="Lab"
parli$party[parli$party=="lab."]="Lab"
parli$party[parli$party=="Lab."]="Lab"
sort(table(parli$party))
##would need to do more sorting of party
p2=parli[parli$party %in% c("L","C","Lab")]& parli$date == "1950-02-23",]

#day3
require(tm)
texts=c("This is a document","This is another document","When is lunch")
vs = VectorSource(texts)
vc=VCorpus(vs) #use PCorpus if huge texts
vc=tm_map(vc, removePunctuation)
dtm=DocumentTermMatrix(vc)
inspect(dtm)

dist(dtm)
clust=hclust(dist(dtm))
plot(clust)

#playing with fed papers
tdmf = read.csv("http://andy.egge.rs/data/fp_stop.csv", as.is = T, row.names = 1)
# vsfed=VectorSource(tdm) #not necessary b/c already a dtm!
# vcfed=VCorpus(vsfed)
# vcfed=tm_map(vcfed,removePunctuation)
# vcfed=tm_map(vcfed,removeNumbers)
# vcfed=tm_map(vcfed,tolower)
# vcfed=tm_map(vcfed,removeWords,stopwords('english'))
# vcfed=tm_map(vcfed,stripWhitespace)
# vcfed=tm_map(vcfed,PlainTextDocument)
#transpose to take tdm to dtm
dtmf=t(tdmf)
clust1=hclust(dist(dtmf))
plot(clust1)

#naivebayes
# require(quanteda)
install_github("kbenoit/quanteda")
dd=parli[parli$party %in% c("C","Lab"),]
m=dfm(dd$bio, ignoredFeatures = stopwords('english'))
tdfm=trim(m,minDoc=3)
nb=textmodel(x=tdfm,y=dd$party, prior='docfreq',model="NB")
predictions=predict(nb, newdata = tdfm)

#felix's lecture
setwd("/Users/jda43/Dropbox/Oxbridge info/Spring School/CompTextAnalysis/test")
d=read.csv("THC_candidates.csv",stringsAsFactors=F)
textpar=d$bio
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
# CorpusTM=tm_map(CorpusTM,PlainTextDocument)
TDM=TermDocumentMatrix(CorpusTM)
dim(TDM)
findFreqTerms(TDM,lowfreq=4,highfreq=10)

TDMS=removeSparseTerms(TDM,0.99)
dim(TDMS)
out= readCorpus(TDMS, type="slam")
names = paste(d$constituency.name,d$sname)
out=prepDocuments(out$documents, out$vocab, names)
summary(out)
TMResult=stm(out$documents, out$vocab, K=8, max.em.its = 75, data = out$meta)
save(TMResult, file="TMResult.RData")
load("TMResult.RData")

plot.STM(TMResult, type="summary", xlim=c(0,2))
mod.out.corr=topicCorr(TMResult)
library(igraph)
plot.topicCorr(mod.out.corr)

toLDAvis(mod=TMResult, out$documents)
