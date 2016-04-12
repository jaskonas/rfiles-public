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
