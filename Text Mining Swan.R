
setwd("W:/Health Economics/Ongoing Publications/Member Satisfaction Survey/TextMining")
getwd()

## clear the decks
rm()

filename <- "UnderperformanceVIC"
filetype <- ".TXT"
file <- paste0(filename, filetype)

data <- readLines(file) # from: MSS Survey

data

df <- data.frame(data)

df

textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)    # this line removed to maintain underscores
# 
# textdata <- gsub("[[:punct:]]", "", textdata)  # don't want to remove spaces
textdata <- gsub("[[:digit:]]", "", textdata)
textdata <- gsub("http\\w+", "", textdata)
textdata <- gsub("[ \t]{2,}", "", textdata)
textdata <- gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
   try_error = tryCatch(tolower(x), error=function(e) e)
   if (!inherits(try_error, "error"))
     y = tolower(x)
   return(y)
 }
 textdata <- sapply(textdata, try.error)
 textdata <- textdata[!is.na(textdata)]
 names(textdata) = NULL

 install.packages(tm) # if not installed
library(tm)
textdata <- removeWords(textdata, stopwords("english"))
textdata

library(wordcloud)
corpus <- Corpus(VectorSource(textdata))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq=v)

#create a filename for the wordcloud
filenamePNG <- paste0(filename,'.PNG')
filenamePNG

#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(d$word,d$freq, min.freq=5)

png(filenamePNG, width=600,height=400)
wordcloud(d$word,d$freq, min.freq=5)
dev.off()

d
export <- data.frame(freq=v,decreasing=true)
# export

export <- data.frame(freq=v)
export
write.table(export, paste0(filename, "Frequencies.txt"), sep="\t", quote=FALSE, col.names = TRUE)