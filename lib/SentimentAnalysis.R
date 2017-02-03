library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)
library(topicmodels)
library(qdap)
library(readr)
library(calibrate)
setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Applied Data Science/Project 1/Spr2017-Proj1-vikasarun/lib")

#Fetching data
folder.path="../data/InauguralSpeeches/"
speech.list=list.files(path = folder.path, pattern = "*.txt")

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

#Donald Trumps most positive words
afinn_strong_sentiments = get_sentiments("afinn")
afinn_strong_sentiments = filter(afinn_strong_sentiments, abs(score) >= 2)
colnames(afinn_strong_sentiments) = c("term", "score")
full.td = group_by(full.td, document)
full.td.score = inner_join(full.td, afinn_strong_sentiments)
full.td.score = arrange(full.td.score, desc(score))
as.data.frame(filter(full.td.score, document == "inaugDonaldJTrump-1.txt"))

#Trying to do the same for all speeches
afinn_strong_sentiments = get_sentiments("afinn")
afinn_strong_sentiments = filter(afinn_strong_sentiments, abs(score) >= 2)
colnames(afinn_strong_sentiments) = c("term", "score")
full.td = group_by(full.td, document)
full.td.score = inner_join(full.td, afinn_strong_sentiments)
full.td.score = arrange(full.td.score, desc(count))
full.td.score = slice(full.td.score, 1:10)
full.scores = summarise(full.td.score, sum(count*score)/sum(count))

#Creating Lit of Dates
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

#Adding Dates to score data gram
full.scores$date = dates
colnames(full.scores) = c("document", "score", "date")
full.scores$date = as.Date(full.scores$date, format = "%m/%d/%Y")
full.scores = arrange(full.scores, date)

plot(full.scores$date, full.scores$score)
textxy(full.scores$date[54:58], full.scores$score[54:58],  substr(full.scores$document,6, nchar(full.scores$document)-4)[54:58], offset=0, pos =1)

#Find second term presidents
second.term.bool = grepl("*2.txt", full.scores$document)
second.term.scores = full.scores
second.term.scores$termIndicator = second.term.bool
second.term.scores = filter(second.term.scores, termIndicator == TRUE)
second.term.names = substr(second.term.scores$document, 6, nchar(second.term.scores$document)-6)
points(second.term.scores$date, second.term.scores$score, pch = 3, col = "Red", labels = second.term.names)

#Splits speeches into sentences
full.text = as.data.frame(matrix(nrow=length(speech.list), ncol = 2))
colnames(full.text) = c("text", "filename")
for(i in 1:length(speech.list))
{
  current.path = paste(folder.path, speech.list[i], sep = "")
  full.text[i,1] = read_file(current.path)
}
full.text[,2] = speech.list


sentence.list=NULL
for(i in 1:length(speech.list))
{
  sentences=sent_detect(full.text$text[i],
                        endmarks = c("?", ".", "!", "|",";"))
  if(length(sentences)>0)
  {
    # colnames(emotions)=paste0("emo.", colnames(emotions))
    # in case the word counts are zeros?
    sentence.list=rbind(sentence.list, 
                        cbind(speech.list[i],
                              sentences=as.character(sentences), 
                              sent.id=1:length(sentences)
                        )
    )
  }
}
colnames(sentence.list) = c("filename", "sentences", "sent.id")
sentence.list = as.data.frame(sentence.list)

#Create Corpus of Sentence Snippets
corpus.list=sentence.list[2:(nrow(sentence.list)-1), ]
sentence.pre=sentence.list$sentences[1:(nrow(sentence.list)-2)]
sentence.post=sentence.list$sentences[3:(nrow(sentence.list)-1)]
corpus.list$snipets=paste(sentence.pre, corpus.list$sentences, sentence.post, sep=" ")
rm.rows=(1:nrow(corpus.list))[corpus.list$sent.id==1]
rm.rows=c(rm.rows, rm.rows-1)
corpus.list=corpus.list[-rm.rows, ]
docs <- Corpus(VectorSource(corpus.list$snipets))
#writeLines(as.character(docs[[sample(1:nrow(corpus.list), 1)]]))

#Clean Corpus
docs <-tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs,stemDocument)

#Create DTM
dtm <- DocumentTermMatrix(docs)
#convert rownames to filenames#convert rownames to filenames
rownames(dtm) <- paste(corpus.list$type, corpus.list$File,corpus.list$Term, corpus.list$sent.id, sep="_")
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm  <- dtm[rowTotals> 0, ]
corpus.list=corpus.list[rowTotals>0, ]

#Perform Topic Model
k = 10
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, 
                                                  seed = seed, best=best,
                                                   iter = iter 
                                                  ))

#Write Reulsts
ldaOut.topics <- as.matrix(topics(ldaOut))
table(c(1:k, ldaOut.topics))
write.csv(ldaOut.topics,file=paste("../output/LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,20))
write.csv(ldaOut.terms,file=paste("../output/LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("../output/LDAGibbs",k,"TopicProbabilities.csv"))

terms.beta=ldaOut@beta
terms.beta=scale(terms.beta)
topics.terms=NULL
for(i in 1:k){
  topics.terms=rbind(topics.terms, ldaOut@terms[order(terms.beta[i,], decreasing = TRUE)[1:7]])
}
topics.terms
ldaOut.terms



#Assinging topic names to topics.The topic groupings are always the same, but they are numbered differently each time the code is run.
topic.key.words = c("econom", "offic", "patriot","war", "equal", "democraci", "law","opportun", "futur", "foreign")
topic.list=c("Economy", "Government", "Patriotism", "Defense", "Equality", "American Ideals", "Legislation","Citizen Actions", "Destiny", "Foreign Affairs")
topic.name = MatchTopics(ldaOut.terms, topic.key.words,topic.list)

for(i in 1:length(topic.name))
{
  PlotTopicTimeSeries(corpus.list, topicProbabilities, i, topic.name[i], dates)
}

#Measuring Economy importance in speeches
economic.speech.measure = as.data.frame(cbind(corpus.list, topicProbabilities[,1]))
colnames(economic.speech.measure) = c("filename", "sentences", "sent.id", "snipets", "probability")
economic.speech.measure = group_by(economic.speech.measure, filename)
economic.speech.measure = summarise(economic.speech.measure, mean(probability))
economic.speech.measure$date = dates
colnames(economic.speech.measure)  = c("filename", "probability", "date")
economic.speech.measure$date = as.Date(economic.speech.measure$date, format = "%m/%d/%Y")
economic.speech.measure = arrange(economic.speech.measure, date)
plot(economic.speech.measure$date, economic.speech.measure$probability)

PlotTopicTimeSeries(corpus.list, topicProbabilities, 1, "Economy", dates)

economic.speech.measure = as.data.frame(cbind(corpus.list, topicProbabilities[,6]))
colnames(economic.speech.measure) = c("filename", "sentences", "sent.id", "snipets", "probability")
economic.speech.measure = group_by(economic.speech.measure, filename)
economic.speech.measure = summarise(economic.speech.measure, mean(probability))
economic.speech.measure$date = dates
colnames(economic.speech.measure)  = c("filename", "probability", "date")
economic.speech.measure$date = as.Date(economic.speech.measure$date, format = "%m/%d/%Y")
economic.speech.measure = arrange(economic.speech.measure, date)
plot(economic.speech.measure$date, economic.speech.measure$probability)

#Trying the same thing with 15 topics
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 15

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, 
                                                  seed = seed, best=best,
                                                  burnin = burnin, iter = iter, 
                                                  thin=thin))
#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
table(c(1:k, ldaOut.topics))
write.csv(ldaOut.topics,file=paste("../out/LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,20))
write.csv(ldaOut.terms,file=paste("../out/LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("../out/LDAGibbs",k,"TopicProbabilities.csv"))

terms.beta=ldaOut@beta
terms.beta=scale(terms.beta)
topics.terms=NULL
for(i in 1:k){
  topics.terms=rbind(topics.terms, ldaOut@terms[order(terms.beta[i,], decreasing = TRUE)[1:7]])
}
topics.terms
ldaOut.terms

topics.hash=c("Economy", "America", "Defense", "Belief", "Election", "Patriotism", "Unity", "Government", "Reform", "Temporal", "WorkingFamilies", "Freedom", "Equality", "Misc", "Legislation")

for(i in 1:length(topics.hash))
{
  PlotTopicTimeSeries(corpus.list, topicProbabilities, i, topics.hash[i], dates)
}

#PLotting topic assignments per president
CreatePresidentPieCharts_15(corpus.list, topicProbabilities, dates, topics.hash)

#AddingPartyAssignments
party.list = read.csv("../data/InaugurationInfo.csv", header=TRUE)
party.list = mutate(party.list, name = paste(File, Term, sep="-"))
party.list = dplyr::select(party.list, Party, name)
party.list = arrange(party.list, name)

CreatePartyCharts(corpus.list, party.list, topicProbabilities, topics.hash, dates)

#Converting topic numbers to topic names
ldaOut.topicnames = ldaOut.topics
for(i in 1:nrow(ldaOut.topicnames))
{
  ldaOut.topicnames = topics.hash[ldaOut.topics]
}