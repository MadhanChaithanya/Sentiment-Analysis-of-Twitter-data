library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
##SENTIMENT FUNCTION
##NEW
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
##NEW

pos.words = scan('C:/Users/gvsnk/Desktop/Sentiment Analysis/SA/positive-words.txt',what='character',comment.char=';')
neg.words = scan('C:/Users/gvsnk/Desktop/Sentiment Analysis/SA/negative-words.txt',what='character',comment.char=';')
bscore <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress='text')
rscore <- score.sentiment(tweet2_df$text,pos.words,neg.words,.progress='text')
hist(rscore$score)
hist(bscore$score)
consumerKey <- "ItiQRj77FbqEzktRPxBMHgrTp"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "AEZvXxVTTu5T3IlI7CLxPHT93RSe6F0GfswBxO7GubfMQsAITy"
accessToken <- "2558327809-1UfBNyIpQ4oszoPCdhAMfS8fUZM1pu3Fy8FXk65"
accessTokenSecret <- "zDGhToczecEEHOcJyG4FJET48h888HJbZzfQ5jrQLeE4T"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
  setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
  tweet1 <- userTimeline("@barcalona",n=100)
  tweet2 <- userTimeline("@realmadriden",n=100)
  tweet_df <- tbl_df(map_df(tweet1,as.data.frame))
  tweet2_df <- tbl_df(map_df(tweet2,as.data.frame))
  
  
  