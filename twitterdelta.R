# tutorial how to scrape twitter. 
#json gives problems. maybe with rjson better results. 
library(plyr)

library(twitteR)
#library(RJSONIO)
#library(rjson)
library(ROAuth)
#library(RCurl)
#if you need to authentify
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "http://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
consumerKey = "yourtwitterusername"
consumerSecret = yourtwitterpasword # type it here.

#ratelimiting may be the problem. 
getURL( "https://api.twitter.com/1/account/rate_limit_status.json") # to know the blacklist status of tyour IP/login. 

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred
#to authorize an app, get an app auth token and 
#To enable the connection, please direct your web browser to: 
#  https://api.twitter.com/oauth/authorize?oauth_token=xxxx
#When complete, record the PIN given to you and provide it here: xxxxxx

#
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.perm")
twitCred$handshake()#cainfo="cacert.perm")
registerTwitterOAuth(twitCred)

rateLimit<-   rateLimitInfoFactory$show()
  
rateLimit$hourlyLimit

publicTimeline()
delta.tweets = searchTwitter('@delta', n=1000,lang='en')
ratelimit.url<- "https://api.twitter.com/1/account/rate_limit_status.json" # to know the blacklist status of tyour IP/login. 
getURL( ratelimit.url) # to know the blacklist status of tyour IP/login. 

length(delta.tweets)

class(delta.tweets)
?status
tweet = delta.tweets[[1]]
class(tweet)
tweet$getScreenName()
tweet$getText()

delta.text = ldply(delta.tweets, function(t) t$getText() )
length(delta.text)[1] #1500
head(delta.text, 5)

#Load sentiment word lisTS
#Download Hu & Liu’s opinion lexicon:
 # http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
hu.liu.pos = scan('opinion-lexicon-English/positive-words.txt',
                  what='character', comment.char=';')
hu.liu.neg = scan('opinion-lexicon-English/negative-words.txt',
                  what='character', comment.char=';')
#Add a few industry-speciﬁc and/or especially emphatic terms:
pos.words = c(hu.liu.pos, 'upgrade','luv')
neg.words = c(hu.liu.neg, 'wtf', 'wait',
              'waiting', 'epicfail', 'mechanical','h8')
#example use 
sample = c("You're awesome and I love you",
           "I hate and hate and hate. So angry. Die!",
           "Impressed and amazed: you are peerless in your achievement of 
unparalleled mediocrity.")
result = score.sentiment(sample, pos.words, neg.words)
result
#[1]  2 -5  3
rm(result) #So, not so good with sarcasm. Here are a couple of real tweets:

score.sentiment(data.frame(text=c("@Delta I'm going to need you to get it together.
Delay on tarmac, delayed connection, crazy gate changes... #annoyed",
                      "Surprised and happy that @Delta helped me avoid the 3.5 hr layover I
was scheduled for.  Patient and helpful agents. #remarkable")),
                    pos.words, neg.words)


delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
names(delta.scores)<-'score'
delta.scores$airline <- 'Delta'
delta.scores$code <- 'DL'
hist(delta.scores$score)
summary(delta.scores$score)
library(ggplot2)
qplot(delta.scores$score)

#automate the two together
tweetscores<- function (keyword= '@ExperienceMSM', n=1000, .progress='text'){
  res.text=llply(searchTwitter(keyword, n), function(t) t$getText() )
  return(score.sentiment(res.text, pos.words,
                         neg.words, .progress=.progress))
}
tweetdf<- function(df, n=1000,.progress='text'){
  data.frame(cbind(scores=tweetscores(df$kw, n, .progress),
                   kw=df$kw, name=df$name,code=df$code))}

MSM<-tweetscores()
tweetscores('@maastricht')
head(MSM)
tweetDF2df<- function(df=data.frame(kw='@vincentfeltkamp', name='', code=''),n=150,.progress='text' ){
  ddply(.data=df, .variables='kw',
        .fun=function (df){ tweetdf(df,n,.progress)} 
  )
}

interest<- data.frame(
  kw=c('@AirFrance', '@americanAir','@UnitedAirlines','@USairways','@ContinentalAir',
       '@SouthwestAir','@JetBlue','@KLM','@Delta'), 
  name=c('AirFrance','American','United','USairways','Continental',
         'Southwest','JetBlue','KLM', 'Delta'),
  code=c('AF','AA','UA','US','CO',
         'WS', 'JB','KLM','DL')
)
View(interest) # or the next one if you have rstudi
rstudio::viewData(interest)
#works first time. then defaults to no json. Twitter overflow? 
allscores<- tweetDF2df(interest)



library(ggplot2)
qplot(delta.scores$score)


all.scores = rbind( american.scores, continental.scores, delta.scores,
jetblue.scores, southwest.scores, united.scores, us.scores )

ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=airline), binwidth=1) +
  facet_grid(airline~.) + # make a separate plot for each airline
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )
twitter.df = ddply(all.scores, c('airline', 'code'), summarise,
                   pos.count = sum( very.pos ), neg.count = sum( very.neg ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count /
                            twitter.df$all.count )
orderBy(~-score, twitter.df)

library(XML)
acsi.url = 'http://www.theacsi.org/index.php?
option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'
acsi.df = readHTMLTable(acsi.url, header=T, which=1, 
                          stringsAsFactors=F)
# only keep column #1 (name) and #18 (2010 score)
acsi.df = acsi.df[,c(1,18)]
head(acsi.df,1)
#10
#1 Southwest Airlines 79

colnames(acsi.df) = c('airline', 'score')
acsi.df$code = c('WN', NA, 'CO', NA, 'AA', 'DL', 
                   'US', 'NW', 'UA')

acsi.df$score = as.numeric(acsi.df$score)


compare.df = merge(twitter.df, acsi.df, by='code',
                   suffixes=c('.twitter', '.acsi'))
#Unless you specify “all=T”, non-matching rows are dropped (like a SQL
#INNER JOIN), and that’s what happened to top scoring JetBlue.
compare.df = subset(compare.df, all.count > 100) #continental is going to disappear

ggplot( compare.df ) +
  geom_point(aes(x=score.twitter, 
                 y=score.acsi,
                 color=airline.twitter), size=5) +
  geom_smooth(aes(x=score.twitter,
                  y=score.acsi, group=1), se=F,
              method="lm") +
  theme_bw() +
  opts(legend.position=c(0.2, 
                         0.85))

??tm  # for serious textmining
?slam




score.sentiment <- function(sentences, pos.words, neg.words, .progress='text')
{ require(stringr)
  scores = ldply(sentences, function(sentence, pos.words, neg.words) {  
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress ) 
  return (scores) #scores.df = data.frame(score=scores, text=sentences,stringsAsFactors =FALSE)
  #return(scores.df)
}


cleantext<- function(sentences, .progress='text'){
  llply(sentences, function(sentence) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
  },.progress=.progress)
}
