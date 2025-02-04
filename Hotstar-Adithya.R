---
  title: "Text Mining of Hotstar Data"
author: "venkatesh, Sarath, Sandeep, Bhargav, Aditya"
date: "23 March 2020"
output:
  html_document: default
pdf_document: default
---
  
#Use Twitter data on 2016 USOPEN (tennis) to demonstrate text mining and visualization techniques including text cleanup, word cloud, frequent terms, topic modelling and sentiment analysis 

```{r version,include=TRUE}
# Check the r version
R.version
```

```{r setup, include=FALSE}
#Setup the environment

install.packages("topicmodels")
install.packages("qdap")
rm(list=ls())
library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(syuzhet)
library(qdap)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
```

#####Read Twitter Data

```{r}
# Set directory and read data
setwd("C:/GLIM/Term VII/WSMA/")
tweets.df <- read.csv("HotstarTwitterData.csv")

# Convert char date to correct date format
tweets.df$timestamp <- as.Date(tweets.df$timestamp, format= "%d-%m-%y")
tweets.df$tweet_text <- as.character(tweets.df$tweet_text)
str(tweets.df)

```

#####Cleaning the text data by removing links, tags and delimiters.   
#####Build a Corpus, and specify the location to be the character Vectors  
```{r}

# Remove character string between < >
tweets.df$tweet_text <- genX(tweets.df$tweet_text, " <", ">")

# Create document corpus with tweet text
myCorpus<- Corpus(VectorSource(tweets.df$tweet_text)) 

```

#####convert to Lowercase  
```{r}
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))
writeLines(strwrap(myCorpus[[750]]$content,60))
```

#####Remove the links (URLs)  
```{r}
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
writeLines(strwrap(myCorpus[[750]]$content,60))
```
#####Remove the @ (usernames)  
```{r}
removeUsername <- function(x) gsub("@[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeUsername))
writeLines(strwrap(myCorpus[[750]]$content,60))
```


#####Remove anything except the english language and space  
```{r}
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
writeLines(strwrap(myCorpus[[750]]$content,60))
```

#####Remove Stopwords  
```{r}
myStopWords<- c((stopwords('english')),c("hotstar", "done", "go", "har", "th","a","oy","oyoy","get", "fir", "itâs", "tv", "also", "ðÿðÿðÿðÿðÿðÿâ", "bollywoodâs", "ðÿðÿ", "ðÿðÿðÿðÿ", "can", "ðÿ", "â", "now", "moâ"))
myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 
writeLines(strwrap(myCorpus[[750]]$content,60))
```

#####Remove Single letter words  
```{r}
removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
writeLines(strwrap(myCorpus[[750]]$content,60))
```

#####Remove Extra Whitespaces  
```{r}
myCorpus<- tm_map(myCorpus, stripWhitespace)
writeLines(strwrap(myCorpus[[750]]$content,60))

```

#####keep a copy of "myCorpus" for stem completion later  
```{r}
myCorpusCopy<- myCorpus
```

#####Stem words in the corpus 

```{r Turning into Corpus, include=TRUE, warning=FALSE}
#myCorpus<-tm_map(myCorpus, stemDocument)
#writeLines(strwrap(myCorpus[[250]]$content,60))
```

#####Function to correct/complete the text after stemming

```{r Datacorrection}
#stemCompletion2 <- function(x,dictionary) {
#  x <- unlist(strsplit(as.character(x)," "))
#  x <- x[x !=""]
#  x <- stemCompletion(x, dictionary = dictionary)
#  x <- paste(x, sep="", collapse=" ")
#  PlainTextDocument(stripWhitespace(x))
#}
```

#####Stem Complete and Display the same tweet above with the completed and corrected text. 

```{r display the tweet}
#myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
#myCorpus <- Corpus(VectorSource(myCorpus))
#writeLines(strwrap(myCorpus[[250]]$content, 60))

```

#####Correcting mis-splet words

```{r Word correction}
#wordFreq <- function(corpus,word)
#{
#  results<- lapply(corpus,
#                   function(x){ grep(as.character(x),pattern = paste0("\\<", #word))})
#  sum(unlist(results))
#}
#n.tenni<- wordFreq(myCorpusCopy, "tenni")
#n.tennis <- wordFreq(myCorpusCopy, "tennis")
#cat(n.tenni, n.tennis)
```

#####Used to replace words with the proper ones
```{r word replacement}
#replaceWord <- function(corpus, oldword, newword)
#{
#  tm_map(corpus, content_transformer(gsub), pattern=oldword, replacement=newword)
#  }
#myCorpus<- replaceWord(myCorpus, "tenni", "tennis")
```
#####Creating a term document matrix
```{r creating tdm}
#myCorpus <- Corpus(VectorSource(myCorpus))
tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
tdm
```

#####Find the terms used most frequently
```{r Term frequency1}
(freq.terms <- findFreqTerms(tdm, lowfreq = 25))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 25)
df <- data.frame(term = names(term.freq), freq= term.freq)
```
#####Frequency analysis

```{r Term frequency2}
(freq.terms <- findFreqTerms(tdm, lowfreq = 10))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 10)
df1 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 55))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 55)
df2 <- data.frame(term = names(term.freq), freq= term.freq)

(freq.terms <- findFreqTerms(tdm, lowfreq = 85))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 85)
df3 <- data.frame(term = names(term.freq), freq= term.freq)

library(Rserve)
Rserve()
save(df2, file="55freq.rda")

```



#####plotting the graph of frequent terms
```{r Graph}
p1=ggplot(df1, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@10", x="Terms", y="Term Counts")) + theme(axis.text.y = element_text(size=10))


p2=ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@25", x="Terms", y="Term Counts"))+
  theme(axis.text.y = element_text(size=10))


p3=ggplot(df2, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@55", x="Terms", y="Term Counts"))

p4=ggplot(df3, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="@85", x="Terms", y="Term Counts")) 

```
#####plotting the graph of frequent terms
```{r Grid1}
grid.arrange(p1,p2,ncol=2)
```

```{r Grid2}
grid.arrange(p3,p4,ncol=2)
```

#####calculate the frequency of words and sort it by frequency and setting up the Wordcloud

```{r WordCloud, warning=FALSE}
# Creating the wordcloud

word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 150)
```

##### Find association with a specific keyword in the tweets - usopen, champion,next
```{r Find Association1}
list1<- findAssocs(tdm, "staggering", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Staggering",border = "black")
```


```{r Find Association2}
list1<- findAssocs(tdm, "disney", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Disney",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "netflix", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Netflix",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "specialops", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Specialops",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "best", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Best",border = "black")
```

```{r Find Association3}
list1<- findAssocs(tdm, "airing", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Airing",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "logo", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Logo",border = "black")
```

```{r Find Association3}
list1<- findAssocs(tdm, "hd", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "HD",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "content", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Content",border = "black")
```


```{r Find Association3}
list1<- findAssocs(tdm, "digital", 0.2)
corrdf1 <- t(data.frame(t(sapply(list1,c))))
corrdf1

barplot(t(as.matrix(corrdf1)), beside=TRUE,xlab = "Words",ylab = "Corr",col = "blue",main = "Digital",border = "black")
```



##### Topic Modelling to identify latent/hidden topics using LDA technique
```{r Topic Modelling}
dtm <- as.DocumentTermMatrix(tdm)

rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 10) # find 10 topics
term <- terms(lda, 10) # first 10 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics<- topics(lda)
topics<- data.frame(date=(tweets.df$created), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")
```

#####Sentiment Analysis: understanding emotional valence in tweets using syuzhet

```{r Sentiment Analysis}
mysentiment<-get_nrc_sentiment((tweets.df$tweet_text))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness",
           "Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis)
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", main = "Twitter sentiment", 
        sub = "US Open", col = colors, border = "black", xpd = F, ylim = yRange,
        axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")

```

#####Sentiment Analysis : Plot by date - understanding cummulative sentiment score movement 
```{r Sentiment Plot1}

mysentimentvalues <- data.frame(get_sentiment((tweets.df$tweet_text)))
colnames(mysentimentvalues)<-"polarity"
mysentimentvalues$date <- tweets.df$timestamp

result <- aggregate(polarity ~ date, data = mysentimentvalues, sum)
result
plot(result, type = "l")

```

#####Sentiment Analysis: Plot by date - understanding average sentiment score movement 
```{r Sentiment Plot2}
result1 <- aggregate(polarity ~ date, data = mysentimentvalues, mean)
result1
plot(result1, type = "l")

```



