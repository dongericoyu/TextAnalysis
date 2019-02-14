setwd("C:/Users/Dong/Desktop/Ongoing Project-R/TwitterSentiment_2018")

#Read File
JF<-read.csv(file.choose(),header=T)
str(JF)




#Build corpus (converting ': not working) 
##回头试试把text先弄出来放到word或者notepad,然后在里面删除’，然后再放回CSV里。
library(tm)
corpus <- iconv(JF$text, to = "utf-8")

corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#BS<-read.csv(file.choose(),header=T)
#str(BS)
#usableText=str_replace_all(BS,"[^[:graph:]]", " ")
#corpus <- iconv(BS$text, to = "utf-8")
#corpus <- Corpus(VectorSource(corpus))
#usableText=str_replace_all(corpus,"[^[:graph:]]", " ")

#save_as_csv(usableText, "usableText", prepend_ids = TRUE, na = "",
#            fileEncoding = "UTF-8")
#dataframe <- data.frame(text=sapply(corpus, identity), 
#                        stringsAsFactors=F)
#write_as_csv(dataframe,"BS_utf8")
#Build corpus (converting ': not working)

#Clean Text
corpus <- tm_map(corpus, tolower) #lower cases

corpus <- tm_map(corpus, removePunctuation) #remove punctures, commas

corpus <- tm_map(corpus, removeNumbers)




inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))

inspect(cleanset[1:5])


removeURL <- function(x) gsub('http[[:alnum:]]*','',x) #remove URL
cleanset <- tm_map(cleanset, content_transformer(removeURL))

inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)

inspect(cleanset[1:5])


#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm 

tdm <- as.matrix(tdm)   #check and see whether there is common words that blur out those sentences
tdm[1:10, 1:20]   

#if taking out words (you can define your own words): cleanset<-tm_map(cleanset, removeWords, c('aapl', 'apple'));


#Bar plot 
w <- rowSums(tdm)
w <- subset(w,w >= 100) #only those words appears more than 25
w

barplot(w,
        las = 2,
        col = rainbow(50) )


#if we want to combine some synonyms, use this: 
#cleanset <- tm_map(cleanset, gsub,
#                   pattern = 'stocks',
#                   replacement = "stock")


#Word Clous
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = T)
set.seed(1988)
wordcloud(words = names(w),
          freq = w,
          #max.words=,
          random.order = F,
          min.freq =100,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(3,0.3))




#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


#read file
JF_sentiment<-read.csv(file.choose(),header=T)
tweets <- iconv(JF_sentiment$text, to = 'utf-8')

#Obtain sentiment scores
s <- get_nrc_sentiment(tweets)
summary(s)
head(s)

tweets[2]
get_nrc_sentiment('')


#Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for JF Tweets')





#load new time format data
JF_sentiment<-read.csv(file.choose(),header=T)


#convert to standard time format in R
JF_sentiment$created_at<-as.Date(JF_sentiment$created_at,format="%m%d%y")

#create query(search) variable
JF_sentiment$query <- "trade"
df<-JF_sentiment #if using more than one data set(another CSV file), row bind into single dataframe
                 #df <-rbine(JF_sentiment, BS, JM)


#obtain sentiment scores
tweets <- iconv(JF_sentiment$text, to = 'utf-8')

#Obtain sentiment scores
sa <- get_nrc_sentiment(tweets)
summary(sa)
head(sa)


#view output
tibble::as_tibble(sa)


#combining sentiment analysis and original dataframe
df <- cbind(df,sa)


## create function for aggregating date-time vectors
round_time <- function(x, interval = 60) {
  ## round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  ## center so value is interval mid-point
  rounded <- rounded + round(interval * .5, 0)
  ## return to date-time
  as.POSIXct(rounded, origin = "2000-01-01")
}

## use pipe (%>%) operator for linear syntax
long_emotion_ts <- df %>%
  ## select variables (columns) of interest
  dplyr::select(created_at, query, anger:positive) %>%
  ## convert created_at variable to desired interval
  ## here I chose 720 hour intervals (3 * 60 seconds * 60 mins = 3 hours)
 # mutate(created_at = round_time(created_at, 720 * 60 * 60)) %>%
  ## transform data to long form
  tidyr::gather(sentiment, score, -created_at, -query) %>%
  ## group by time, query, and sentiment
  group_by(created_at, query, sentiment) %>%
  ## get mean for each grouping
  summarize(score = mean(score, na.rm = TRUE),
            n = n()) %>%
  ungroup()




## view data
long_emotion_ts


#lims <- as.POSIXct(strptime(c("2011-01-01 03:00","2011-01-01 16:00"), format = "%Y-%m-%d %H:%M"))   



## plot data
long_emotion_ts %>%
  ggplot(aes(x = created_at, y = score, color = query)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ sentiment, scale = "free_y", nrow = 2) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 9),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Sentiment analysis of Twitter statuses over time",
       subtitle = "Tweets aggregated by month on topic on trade") +
       scale_x_date(date_breaks = "1 month", date_labels = "%b %d")





#Create some date and time data:
dates <- as.POSIXct(as.Date("2011/01/01") + sample(0:365, 100, replace=TRUE))
times <- as.POSIXct(runif(100, 0, 24*60*60), origin="2011/01/01")

datetime <- data.frame(
  dates = dates,
  times = times
)

library(ggplot2)
library(scales)
ggplot(datetime, aes(x=dates, y=times)) + 
  geom_point() + 
  scale_y_datetime(breaks=date_breaks("1 month"), labels=date_format("%H:%M")) + 
  theme(axis.text.x=element_text(angle=90))