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