
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(RCurl)
library(XML)

df = data.frame(word=c('AYUSH','AISHWARYA','ASHISH','ARPIT','ANSHI'),freq=c(50,43,25,40,35))
df
wordcloud2(df, size = 1, shape = 'star')



