library(dplyr)

PlotTopicTimeSeries = function (corpus.list, topicProbabilities, topicIndex, topicName, dates) 
{
  speech.measure = as.data.frame(cbind(corpus.list, topicProbabilities[,topicIndex]))
  colnames(speech.measure) = c("filename", "sentences", "sent.id", "snipets", "probability")
  speech.measure = group_by(speech.measure, filename)
  speech.measure = summarise(speech.measure, mean(probability))
  speech.measure$date = dates
  colnames(speech.measure)  = c("filename", "probability", "date")
  speech.measure$date = as.Date(speech.measure$date, format = "%m/%d/%Y")
  speech.measure = arrange(speech.measure, date)
  yString = paste("Estimated percetnage of sentences on ", topicName, sep = "")
  titleString =paste("The Importance of ", topicName, " across Time",sep = "")
  photoString = paste("TopicTimeSeries", ncol(topicProbabilities), ".", topicIndex,".jpg", sep = "")
  photoPath = paste("../figs","/",photoString,sep = "")
  jpeg(filename = photoPath)
  plot(speech.measure$date, speech.measure$probability, xlab = "Year", ylab = yString, main = titleString)
  dev.off()
}

MatchTopics = function(ldaOut.terms, topic.key.words, topic.names)
{
  topic.list = rep(NA,length(topic.key.words))
  for(i in 1:length(topic.key.words))
  {
    index = which(ldaOut.terms == topic.key.words[i], arr.ind = TRUE)
    column = index[1,2]
    topic.list[column] = topic.names[i]
    temp = paste("This is iteration", i)
    print(temp)
  }
  return(topic.list)
}