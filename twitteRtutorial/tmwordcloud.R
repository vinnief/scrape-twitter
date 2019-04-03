require(twitteR)
require(tm)

library("twitteR")
library("reshape2")
library("ggplot2")
library("plyr")

#example of how to do it on XKCD
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[, 3])))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

president<-function(x){
  Romney=searchTwitter('@MittRomney', n=1500)
  Obama=searchTwitter('@BarackObama', n=1500)
  textRomney=laply(Romney, function(t) t$getText())
  textObama=laply(Obama, function(t) t$getText())
  resultRomney=score.sentiment(textRomney, positive.words, negative.words)
  resultRomney$candidate='Romney'
  resultObama=score.sentiment(textObama, positive.words, negative.words)
  resultObama$candidate='Obama'
  result<-merge(resultObama,resultRomney, all=TRUE)
  result$candidate<-as.factor(result$candidate)
  result$time<-date()
  return(result)
}

cresult<-ddply(result, .(candidate),summarise, score.mean=mean(score))
p<-ggplot(result, aes(x=score, fill=candidate)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_vline(data=cresult, aes(xintercept=score.mean,  colour=candidate),
             linetype="dashed", size=1)

#graph of debate 

vp<-function(x){n=1500
  Ryan=searchTwitter('@PaulRyanVP', n)
  Biden=searchTwitter('@JoeBiden', n)
  textRyan=laply(Ryan, function(t) t$getText())
  textBiden=laply(Biden, function(t) t$getText())
  resultRyan=score.sentiment(textRyan, positive.words, negative.words)
  resultRyan$candidate='Ryan'
  resultBiden=score.sentiment(textBiden, positive.words, negative.words)
  resultBiden$candidate='Biden'
  result<-merge(resultBiden,resultRyan, all=TRUE)
  result$candidate<-as.factor(result$candidate)
  result$time<-date()
  return(result)
}

#in a endless loop collecting data during the debate 
debate<-vp()
repeat {
  startTime x<-vp()
  debate<-merge(x, debate, all=TRUE)
  sleepTime 0)
Sys.sleep(sleepTime)
}

x<-subset(debate, !duplicated(text))
x$minute<-strptime(x$time, "%a %b %d %H:%M:%S %Y")
x$minute1<-format(x$minute,"%H:%M")
x<-subset(x, minute1>="21:00")
period<-unique(x$minute1)
period<-period[order(period)]
Biden Ryan mean<-data.frame(period, Biden, Ryan)
dfm ggplot(dfm, aes(period, value, colour=variable, group=variable, xlab="time", ylab="score"))+
  geom_point()+geom_line()+opts(axis.text.x=theme_text(angle=45),
                                axis.ticks = theme_blank(),axis.title.y=theme_blank())




#twitter word graph
p.corpus <- Corpus(DataframeSource(data.frame(as.character(romneypositive[,3]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, c(r,stopwords("english"))))
ap.tdm <- TermDocumentMatrix(ap.corpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("romneypositive.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
