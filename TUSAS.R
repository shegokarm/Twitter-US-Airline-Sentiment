#Importing libraries
libs <- c("RSQLite","dplyr","tm","wordcloud")
lapply(libs, require, character.only = TRUE)

#Importing data
#1st way
tweets <- read.csv("E:/RStudio_Dataset/Twitter US Airline Sentiment/airline-twitter-sentiment/Tweets.csv")

#2nd way
#db <- dbConnect(dbDriver("SQLite"),"E:/RStudio_Dataset/Twitter US Airline Sentiment/airline-twitter-sentiment/database.sqlite")
#tweets <- dbGetQuery(db,"select * from Tweets LIMIT 1000")

#Importing relevant columns
dat <- select(tweets,airline_sentiment,negativereason,airline,text)

#Cleaning data
dat$text = gsub("^@\\w+", "", dat$text)

positive <- subset(dat,airline_sentiment == "positive")
neutral <- subset(dat,airline_sentiment == "neutral")
negative <- subset(dat, airline_sentiment == "negative")

wordsToRemove = c('get', 'cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')

wc <- function(documents){
  corpusnew <- Corpus(VectorSource(documents))
  corpusnew <- tm_map(corpusnew,content_transformer(tolower))
  corpusnew <- tm_map(corpusnew,removePunctuation)
  corpusnew <- tm_map(corpusnew,removeWords,stopwords("english"))
  corpusnew <- tm_map(corpusnew, removeWords,wordsToRemove)
  corpusnew <- tm_map(corpusnew,stripWhitespace)
  dt <- DocumentTermMatrix(corpusnew)
  dt <- as.data.frame(as.matrix(dt))
  return(dt)
}

opt <- wc(negative$text)
words <- colnames(opt)
freq_neg <- colSums(opt)

png("twitter1.png")
wordcloud(words,freq_neg,min.freq = sort(freq_neg, decreasing = TRUE)[[300]],random.order = FALSE,
          random.color = TRUE, colors = brewer.pal(8, "Dark2"))

dev.off()

opt <- wc(neutral$text)
words <- colnames(opt)
freq_neu <- colSums(opt)

png("twitter2.png")
wordcloud(words,freq_neu,min.freq = sort(freq_neu, decreasing = TRUE)[[300]],random.order = FALSE,
          random.color = TRUE, colors = brewer.pal(8, "Dark2"))

dev.off()

opt <- wc(positive$text)
words <- colnames(opt)
freq_pos <- colSums(opt)

png("twitter3.png")
wordcloud(words,freq_pos,min.freq = sort(freq_pos, decreasing = TRUE)[[300]],random.order = FALSE,
          random.color = TRUE, colors = brewer.pal(8, "Dark2"))

dev.off()

