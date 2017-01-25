library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)
#setwd("C:/Users/Vikas/OneDrive/Backup 1-8-17/Documents/Columbia/Senior/Applied Data Science/Project 1/Spr2017-Proj1-vikasarun/lib")

#Fetching data
folder.path="../data/InauguralSpeeches/"
speeches=list.files(path = folder.path, pattern = "*.txt")

#Data cleaning
SpeechCorpus = Corpus(DirSource(folder.path))
SpeechCorpus = tm_map(SpeechCorpus, stripWhitespace)
SpeechCorpus = tm_map(SpeechCorpus, content_transformer(tolower))
SpeechCorpus = tm_map(SpeechCorpus, removeWords, stopwords("english"))
SpeechCorpus = tm_map(SpeechCorpus, removePunctuation)
SpeechCorpus = tm_map(SpeechCorpus, removeWords, character(0))
SpeechCorpus = tm_map(SpeechCorpus, stemDocument)
full.dtm = TermDocumentMatrix(SpeechCorpus)
full.td = tidy(full.dtm)

#Sentiment Analysis on one speech
small.td = filter(full.td, document == "inaugAbrahamLincoln-1.txt")
afinn_strong_sentiments = get_sentiments("afinn")
afinn_strong_sentiments = filter(afinn_strong_sentiments, abs(score) >= 2)
colnames(afinn_strong_sentiments) = c("term", "score")
small.td.score = inner_join(small.td, afinn_strong_sentiments)
small.td.score = arrange(small.td.score, desc(count))
top_10_count = sum(small.td.score$count[1:10])
weighted.score = 0;
for(i in 1:10)
{
  weighted.score = weighted.score + small.td.score$score[i]*small.td.score$count[i]/top_10_count
}

#Trying to do the same for all speeches
afinn_strong_sentiments = get_sentiments("afinn")
afinn_strong_sentiments = filter(afinn_strong_sentiments, abs(score) >= 2)
colnames(afinn_strong_sentiments) = c("term", "score")
full.td = group_by(full.td, document)
full.td.score = inner_join(full.td, afinn_strong_sentiments)
full.td.score = arrange(full.td.score, desc(count))
full.td.score = slice(full.td.score, 1:10)
full.scores = summarise(full.td.score, sum(count*score)/sum(count))

inaug.dates = read.table("../data/InauguationDates.txt", header = TRUE, sep = "\t" )
#Removing whitespace and punctuation to ensure consistent ordering of names
inaug.dates$PRESIDENT = gsub(" ", "", inaug.dates$PRESIDENT, fixed =TRUE)
inaug.dates$PRESIDENT = gsub(".", "", inaug.dates$PRESIDENT, fixed =TRUE)
inaug.dates = arrange(inaug.dates, PRESIDENT)
#Remove John Tyler, Millard Filmore, Andrew Johnson, Chester Arthur, First Teddy Roosevelt, First Calvin Coolidge, First Harry Truman, First LBJ, Gerald Ford,
inaug.dates = inaug.dates[c(-29,-32,-3,-7,-15),]
inaug.dates[c("36","6","18","30"), 2] = c("","","","")
dates = as.vector(as.matrix(t(inaug.dates[-1])))
dates = dates[dates != ""]

full.scores$date = dates
colnames(full.scores) = c("document", "score", "date")
full.scores$date = as.Date(full.scores$date, format = "%m/%d/%Y")
full.scores = arrange(full.scores, date)

plot(full.scores$date, full.scores$score)

second.term.bool = grepl("*2.txt", full.scores$document)
second.term.scores = full.scores
second.term.scores$termIndicator = second.term.bool
second.term.scores = filter(second.term.scores, termIndicator == TRUE)
points(second.term.scores$date, second.term.scores$score, pch = 3, col = "Red")