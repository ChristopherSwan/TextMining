


install.packages("devtools")
require(devtools)
install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")

install.packages("tm")
## Dependent packages are loaded automatically - in this case the dependency is on the NLP (natural language processing) package.

install.packages("SnowballC")
install.packages("sentiment")

library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)


## Check the working directory
getwd()
## "\\\\PSYLOCKE/Users$/swanc/Documents"

setwd("W:/Health Economics/Ongoing Publications/Member Satisfaction Survey/TextMining")
getwd()

# Create a folder containing the text files

## Create the corpus
## the  Corpus() function can read from various sources including a directory.
## docs <- Corpus(DirSource("\\\\PSYLOCKE/Users$/swanc/Documents/TextMining"))

docs <- Corpus(DirSource("W:/Health Economics/Ongoing Publications/Member Satisfaction Survey/TextMining"))

##some information about the newly created corpus:
docs
summary(docs)
## Length Class             Mode
## FunctionsAndServicesRequired.txt                 2      PlainTextDocument list
## GuildUnderperformance.txt                        2      PlainTextDocument list
## ProductsServicesNonEssentialLesserImportance.txt 2      PlainTextDocument list

writeLines(as.character(docs[[1]]))   # first doc in directory
writeLines(as.character(docs[[2]]))   # second...
writeLines(as.character(docs[[3]]))   # third...

## tm provides functions to clean up text, but can merge words...
## Create a content transformer (called 'ReplaceWithSpaces')
ReplaceWithSpaces <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

## use the contrent transformer to eliminate colons etc:
#docs <- tm_map(docs, ReplaceWithSpaces, "-")
#docs <- tm_map(docs, ReplaceWithSpaces, ":")
#docs <- tm_map(docs, ReplaceWithSpaces, ",") 
#docs <- tm_map(docs, ReplaceWithSpaces, ".")   
docs[[3]] <- tm_map(docs[[3]], ReplaceWithSpaces, "/")

# check the results:
writeLines((docs[[1]])   # first doc in directory
# remove the commas
docs[[1]] <- tm_map(docs[[1]], ReplaceWithSpaces, ",")
# Error in UseMethod("tm_map", x) : 
#  no applicable method for 'tm_map' applied to an object of class "c('PlainTextDocument', 'TextDocument')"



# the tm package provides transformations
getTransformations()
# [1] "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"  

# If it all looks good, we can now apply the removePunctuation transformation. This is done as follows:
  
  #Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)

# remove numbers
docs <- tm_map(docs, removeNumbers)

# remove common words like 'a', "and", "the"
docs <- tm_map(docs, removeWords, stopwords("English"))


writeLines(as.character(docs[[1]]))   # first doc in directory
writeLines(as.character(docs[[2]]))   # second...
writeLines(as.character(docs[[3]]))   # third...

# remove other annoying words:
# myStopwords <- c("can", "say","one","way","use","also","howev","tell","will","much","need","take","tend","
# even","like","particular","rather","said","get","well","make","ask","come","end","first","two","help","often","may","might","see","someth","thing","point","post","look","right","now","think","'ve ","'re ")
#remove custom stopwords
# docs <- tm_map(docs, removeWords, myStopwords)

# Stemming truncates words to their base form
# docs <- tm_map(docs, stemDocument)

# transform to lower case (requires wrapping 'lolower' in content_transformer)
docs <- tm_map(docs, content_transformer(tolower))

# remove numbers
docs <- tm_map(docs, removeNumbers)

writeLines(as.character(docs[[1]]))   # first doc in directory
# looking good....

# docs <- gsub(" cpa ", " CPA ", docs)

#### gsub functions (as part of tm package)
# docs = gsub("[[:punct:]]", "", docs)
# docs = gsub("[[:digit:]]", "", docs)
# docs = gsub("http\\w+", "", docs)
# docs = gsub("[ \t]{2,}", "", docs)
# docs = gsub("^\\s+|\\s+$", "", docs)


# strip whitespace
docs <- tm_map(docs, stripWhitespace)


# create Document Term Matrix
dtm <- DocumentTermMatrix(docs)

dtm
# > dtm
# <<DocumentTermMatrix (documents: 3, terms: 1837)>>
#  Non-/sparse entries: 2552/2959
# Sparsity           : 54%
# Maximal term length: 25
# Weighting          : term frequency (tf)


# get the frequence of occurence of each word
freq <- colSums(as.matrix(dtm))
freq

# check that the length = total number of terms
length(freq)
# yep. good.

# sort the frequency in descending order of term count
ord <- order(freq,decreasing = TRUE)

# list the most and least frequently occuring terms
freq[head(ord)]

# the        and  relations industrial       with government 
# 553        354        352        343        314        300 

# silly words haven't been removed.... so, 

docs <- tm_map(docs, removeWords, stopwords("English"))

# re-run the dtm creation

dtm <- DocumentTermMatrix(docs)
# this time there's less words...
## <<DocumentTermMatrix (documents: 3, terms: 1744)>>
##  Non-/sparse entries: 2347/2885
## Sparsity           : 55%
## Maximal term length: 25
## Weighting          : term frequency (tf)


# get the frequence of occurence of each word
wordfrequencies <- colSums(as.matrix(dtm))
wordfrequencies

# check that the length = total number of terms
length(wordfrequencies)
# yep. good.

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(wordfrequencies),wordfrequencies, min.freq=50)




# sort the frequency in descending order of term count
ord <- order(wordfrequencies,decreasing = TRUE)

# list the most and least frequently occuring terms
wordfrequencies[head(ord)]
wordfrequencies[tail(ord)]

wordfrequencies
wordfrequenciesTable <- as.table(wordfrequencies)
wordfrequenciesTable
hist(wordfrequenciesTable)

# find the words which occur most frequently
stuff <- as.matrix(findFreqTerms(dtm,lowfreq=50))
hist(wordfrequencies)

stuff
wordcloud(names(stuff),stuff,min.freq=30)


# graph the word occurences
wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>100), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)



#convert dtm to matrix
m <- as.matrix(dtm)
#write as csv file (optional)
write.csv(m,file="dtmEight2Late.csv")
#shorten rownames for display purposes
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                     +                      substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))
#compute distance between document vectors
d <- dist(m)











## ........
## Start from here:





## Next, we remove nonessential characters 
## such as punctuation, numbers, web addresses, 
## etc from the text, before we begin processing 
## the actual words themselves.  

docs = gsub("[[:punct:]]", "", docs)
docs = gsub("[[:digit:]]", "", docs)
docs = gsub("http\\w+", "", docs)
docs = gsub("[ \t]{2,}", "", docs)
docs = gsub("^\\s+|\\s+$", "", docs)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
docs = sapply(textdata, try.error)
docs = docs[!is.na(docs)]
names(docs) = NULL

## Next, we perform the sentiment analysis, 
## classifying comments using a Bayesian analysis.  
## A polarity of positive, negative, or neutral 
## is determined.  Finally, the comment, emotion, 
## and polarity are combined in a single dataframe.

class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(textdata, algorithm="bayes")
polarity = class_pol[,4]


sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
## Now that we have processed the comments, we can graph the emotions and polarities.

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

## Prepare the data for creating a word cloud.  
## This includes removing common English stop words.

## The tm package offers a number of transformations that ease the tedium of cleaning data. 
## To see the available transformations  type getTransformations() at the R prompt:

getTransformations()
##  "removeNumbers" "removePunctuation" "removeWords" "stemDocument" "stripWhitespace"


emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))


## Create a DOCUMENT TERM MATRIX (DTM)
## a matrix that lists all occurrences of words in the corpus, by document. 
## In the DTM, the documents are represented by rows and the terms (or words) by columns.  
## If a word occurs in a particular document, then the matrix entry for corresponding 
## to that row and column is 1, else it is 0 (multiple occurrences within a document 
## are recorded - that is, if a word occurs twice in a document, it is recorded as 
## "2" in the relevant matrix entry).


tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)


library(wordcloud2)
wordcloud2(data = textdata)

#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)


#.add color -  RcolorBrewer may be required
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2???))


