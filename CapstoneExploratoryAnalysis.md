---
title: "Capstone Exploratory Analysis"
author: "Babita"
date: "5 September 2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

- Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
- Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.


```r
library(dplyr)
library(ggplot2)
library(stringi)
library(tm)
library(RWeka)
library(wordcloud)
```

## Loading Data

Download the data from [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)


```r
blogsFile <- "./data/en_US/en_US.blogs.txt"
blogs<-file(blogsFile,"r")
blog_lines <- readLines(blogs, encoding = "UTF-8", skipNul = TRUE)

twitterFile <- "./data/en_US/en_US.twitter.txt"
twitter<-file(twitterFile,"r")
twitter_lines <- readLines(twitter, encoding = "UTF-8", skipNul = TRUE)

newsFile <- "./data/en_US/en_US.news.txt"
news<-file(newsFile,"r")
news_lines <- readLines(news,encoding = "UTF-8", skipNul = TRUE)
```

## Data Summary


```r
filenames <- c("Blogs", "Twitter", "News")
sizes <- c(file.info(blogsFile)$size/1024*1024,
           file.info(twitterFile)$size/1024*1024,
           file.info(newsFile)$size/1024*1024)
lines <- c(length(blog_lines), length(twitter_lines), length(news_lines))

chars <- sapply(list(blog_lines,twitter_lines,news_lines)
                , stri_stats_general)['Chars',]


close(blogs)
close(twitter)
close(news)

data_summary <- data.frame(FileName = filenames, FileSize = sizes
                           , LinesCount = lines, CharsCount = chars)

data_summary
```

```
##   FileName  FileSize LinesCount CharsCount
## 1    Blogs 210160014     899288  206824382
## 2  Twitter 167105338    2360148  162096241
## 3     News 205811889      77259   15639408
```



## Sampling and Cleaning - Creating Data Corpus

A sampled corpus is created for further processing by sampling 1% data and cleaning the raw data. The data is gone through processes of conversion to lowercase, removal of white spaces, punctuation, stopwords, numbers, etc. to get a clean sampled corpus for further processing.


```r
samplingRate <- 0.01
set.seed(123)

test_data <- c(sample(blog_lines, length(blog_lines) * samplingRate),
              sample(news_lines, length(news_lines) * samplingRate),
              sample(twitter_lines, length(twitter_lines) * samplingRate)
          )
          
testdata <- iconv(test_data, "UTF-8", "ASCII", sub="")
sample_corpus <- VCorpus(VectorSource(testdata))
sample_corpus <- tm_map(sample_corpus, content_transformer(tolower))
sample_corpus <- tm_map(sample_corpus, stripWhitespace)
sample_corpus <- tm_map(sample_corpus, PlainTextDocument)
sample_corpus <- tm_map(sample_corpus, removePunctuation)
sample_corpus <- tm_map(sample_corpus, removeNumbers)
sample_corpus <- tm_map(sample_corpus, removeWords, stopwords("english"))
#sample_corpus <- tm_map(sample_corpus, stemDocument)
```



## Wordcloud Representation


```r
unigram <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
unidtf <- TermDocumentMatrix(sample_corpus, control=list(tokenize=unigram))
uni_tf <- findFreqTerms(unidtf, lowfreq = 100 )
uni_freq <- rowSums(as.matrix(unidtf[uni_tf,]))
uni_freq <- data.frame(words=names(uni_freq), frequency=uni_freq)
wordcloud(words=uni_freq$words, freq=uni_freq$frequency, max.words=100, colors = brewer.pal(8, "Dark2"))
```

![](CapstoneExploratoryAnalysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Creating Bigrams and Trigrams


```r
bigram <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))

bidtf <- TermDocumentMatrix(sample_corpus, control=list(tokenize=bigram))
tridtf <- TermDocumentMatrix(sample_corpus, control=list(tokenize=trigram))
                             
bi_tf <- findFreqTerms(bidtf, lowfreq = 50 )
tri_tf <- findFreqTerms(tridtf, lowfreq = 10 )

bi_freq <- rowSums(as.matrix(bidtf[bi_tf, ]))
bi_freq <- data.frame(words=names(bi_freq), frequency=bi_freq)

tri_freq <- rowSums(as.matrix(tridtf[tri_tf, ]))
tri_freq <- data.frame(words=names(tri_freq), frequency=tri_freq)
```


## Ploting Top N-grams

1. Top Unigrams

Single top 30 words that are reapeated most frequently


```r
plot_freq <- ggplot(data = uni_freq[order(-uni_freq$frequency),][1:30, ], aes(x = reorder(words, -frequency), y=frequency)) +
              geom_bar(stat="identity", fill="lightblue") +
              theme(axis.text.x = element_text(angle = 45)) +
              ggtitle("Top Unigram") + xlab("words") +  ylab("frequency")

plot_freq
```

![](CapstoneExploratoryAnalysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


2. Top Bigrams

Pair of top 30 words that are reapeated most frequently


```r
plot_freq <- ggplot(data = bi_freq[order(-bi_freq$frequency),][1:30, ], aes(x = reorder(words, -frequency), y=frequency)) +
  geom_bar(stat="identity", fill="skyblue") + theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Top Bigram") + xlab("words") +  ylab("frequency")
  
plot_freq
```

![](CapstoneExploratoryAnalysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3. Top Trigrams

Group of 3 top 20 words that are reapeated most frequently


```r
plot_freq <- ggplot(data = tri_freq[order(-tri_freq$frequency),][1:20, ], aes(x = reorder(words, -frequency), y=frequency)) +
  geom_bar(stat="identity", fill="blue") + theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Top Trigram") + xlab("words") +  ylab("frequency")

plot_freq
```

```
## Warning: Removed 5 rows containing missing values (position_stack).
```

![](CapstoneExploratoryAnalysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


