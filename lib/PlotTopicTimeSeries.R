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
  titleString =paste("The Importance of Topic ", topicIndex, " across Time",sep = "")
  photoString = paste("TopicTimeSeries", ncol(topicProbabilities), ".", topicIndex,".jpg", sep = "")
  photoPath = paste("../figs","/",photoString,sep = "")
  jpeg(filename = photoPath)
  plot(speech.measure$date, speech.measure$probability, xlab = "Year", ylab = yString, main = titleString)
  dev.off()
}

CreatePresidentPieCharts_15 = function(corpus.list, topicProbabilities, dates, topicNames)
{
  corpus.with.topic = as.data.frame(cbind(corpus.list, topicProbabilities))
  numTopic = ncol(topicProbabilities)
  grouped.names = rep(NA, numTopic + 4)
  grouped.names[1:4] = c("filename", "sentences", "sent.id", "snipets")
  for(i in 1:numTopic)
  {
    grouped.names[(i+4)] = paste("Topic",i, sep = "")
  }
  corpus.with.topic = group_by(corpus.with.topic, filename)
  colnames(corpus.with.topic) = grouped.names
  topic.dist = summarise(corpus.with.topic, mean(Topic1), mean(Topic2), mean(Topic3), mean(Topic4), mean(Topic5), mean(Topic6), mean(Topic7), mean(Topic8), mean(Topic9), mean(Topic10), mean(Topic11), mean(Topic12), mean(Topic13), mean(Topic14), mean(Topic15))
  for(i in 1:nrow(topic.dist))
  {
    temp = as.character(topic.dist$filename[i])
    pres.name = substr(temp, 6, nchar(temp)-4)
    photo.name = paste("TopicPieChart.",pres.name,".jpg", sep="")
    photo.path = paste("../figs/", photo.name, sep = "")
    jpeg(filename = photo.path)
    chart.name = paste("Topic Breakdown for Speech ", pres.name, sep = "")
    pie(as.numeric(topic.dist[i,2:16]), labels = topicNames, col = rainbow(length(topicNames)), main = chart.name)
    dev.off()
    leg.options = c(.5)
    names(leg.options) = c("cex")
    photo.name = paste("TopicBarPlot.",pres.name,".jpg", sep="")
    photo.path = paste("../figs/", photo.name, sep = "")
    jpeg(filename = photo.path)
    barplot(as.numeric(topic.dist[i,2:16]), legend.text = topicNames, col = rainbow(length(topicNames)), main = chart.name, ylim = c(0,0.15), args.legend = leg.options)
    dev.off()
  }
}

CreatePartyCharts = function(corpus.list, party.list, topicProbabilties, topicNames, dates)
{
  corpus.with.topic = as.data.frame(cbind(corpus.list, topicProbabilities))
  numTopic = ncol(topicProbabilities)
  grouped.names = rep(NA, numTopic + 4)
  grouped.names[1:4] = c("filename", "sentences", "sent.id", "snipets")
  for(i in 1:numTopic)
  {
    grouped.names[(i+4)] = paste("Topic",i, sep = "")
  }
  corpus.with.topic = group_by(corpus.with.topic, filename)
  colnames(corpus.with.topic) = grouped.names
  topic.dist = summarise(corpus.with.topic, mean(Topic1), mean(Topic2), mean(Topic3), mean(Topic4), mean(Topic5), mean(Topic6), mean(Topic7), mean(Topic8), mean(Topic9), mean(Topic10), mean(Topic11), mean(Topic12), mean(Topic13), mean(Topic14), mean(Topic15))
  topic.dist = arrange(topic.dist, filename)
  topic.dist = cbind(topic.dist, party.list)
  topic.dist = group_by(topic.dist, Party)
  topic.dist = filter(topic.dist, Party == "Republican" | Party == "Democratic")
  topic.dist = dplyr::select(topic.dist, -(name))
  party.data.names = grouped.names[c(-2,-3,-4)]
  party.data.names = append(party.data.names, "Party")
  colnames(topic.dist) = party.data.names
  topic.dist = summarise(topic.dist, mean(Topic1), mean(Topic2), mean(Topic3), mean(Topic4), mean(Topic5), mean(Topic6), mean(Topic7), mean(Topic8), mean(Topic9), mean(Topic10), mean(Topic11), mean(Topic12), mean(Topic13), mean(Topic14), mean(Topic15))
  leg.label = rep(NA, 15)
  for(i in 1:length(leg.label))
  {
    leg.label[i] = paste("Topic ", i, sep = "")
  }
  for(i in 1:2)
  {
    photo.name = paste("PartyBarPlot.",topic.dist$Party[i],".jpg", sep="")
    photo.path = paste("../figs/", photo.name, sep = "")
    jpeg(filename = photo.path)
    chart.name = paste(topic.dist$Party[i], " Topic Emphasis", sep = "")
    leg.options = list(0.7,20,0.117,"n")
    names(leg.options) = c("cex", "x", "y", "bty")
    barplot(as.numeric(topic.dist[i,2:16]), legend.text = leg.label, col = rainbow(length(topicNames)), main = chart.name, ylim = c(0,0.1), args.legend = leg.options)
    dev.off()
  }
  
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