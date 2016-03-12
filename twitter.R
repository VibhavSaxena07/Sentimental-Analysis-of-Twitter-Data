library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(wordcloud)

install.packages('base64enc')
library(base64enc)


#Twitter Authentication 
key="YOUR KEY"
secret="YOUR SECRET"
accessToken = "YOUR TOKEN"
accessTokenSecret = "YOUR TOKEN SECRET"

twitCred <- setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


N=2000  # tweets to request from each query
S=200  # radius in miles
lats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9, 28.4)

lons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,-104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93, 81.2)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul, Orlando


GOP=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('MUSLIM',lang="en",n=N,since='2015-12-01', until='2015-12-31' ,geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))


GOPlat=sapply(GOP, function(x) as.numeric(x$getLatitude()))
GOPlat=sapply(GOPlat, function(z) ifelse(length(z)==0,NA,z))  

GOPlon=sapply(GOP, function(x) as.numeric(x$getLongitude()))
GOPlon=sapply(GOPlon, function(z) ifelse(length(z)==0,NA,z))  



GOPDate=lapply(GOP, function(x) x$getCreated())
GOPDate=sapply(GOPDate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

GOPText=sapply(GOP, function(x) x$getText())
GOPText=unlist(GOPText)

isretweet=sapply(GOP, function(x) x$getIsRetweet())
retweeted=sapply(GOP, function(x) x$getRetweeted())
retweetcount=sapply(GOP, function(x) x$getRetweetCount())

favoritecount=sapply(GOP, function(x) x$getFavoriteCount())
favorited=sapply(GOP, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=GOPText,date=GOPDate,lat=GOPlat,lon=GOPlon,isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))

usableText=str_replace_all(data$tweet,"[^[:graph:]]", " ") 
# Create corpus
corpus=Corpus(VectorSource(usableText))
# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

#remove misc words and twitter handles

#corpus = tm_map(corpus,removeWords,c("...","&amp;","https","t/","@chica") )

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)



col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,random.color=T, max.word=45, random.order=F,colors=col)



#Getting address of the tweets
data=filter(data, !is.na(lat),!is.na(lon))
lonlat=select(data,lon,lat)

result <- do.call(rbind, lapply(1:nrow(lonlat),function(i) revgeocode(as.numeric(lonlat[i,1:2]))))
result[1:5,]

#Find out city, state and zip code
data2=lapply(result,  function(x) unlist(strsplit(x,",")))
address=sapply(data2,function(x) paste(x[1:3],collapse=''))
city=sapply(data2,function(x) x[2])
stzip=sapply(data2,function(x) x[3])
zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
state=str_extract(stzip,"[:alpha:]{2}")
data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))

#concatenate Data2 to Data
data=cbind(data,data2)

#Clean up the text
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet)
data$tweet=tweet


positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")


#Sentimental Analysis

sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positives)
                   negative.matches = match(words, negatives)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive.matches)
                   negative_matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}

score = sentiment_scores(tweet, positives, negatives, .progress='text')
data$score=score
data$score
score
hist(score,xlab=" ",main="Sentiment of sample tweets\n that have Humanity in them ", border="black",col="skyblue")

write.csv(data,file = "Muslims_Sentiment.csv")
