#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

#read file
BS_sentiment<-read.csv(file.choose(),header=T)
BS_sentiment[,1]=as.Date(BS_sentiment[,1],"%m/%d/%Y %H:%M")


gop_sentiment<-read.csv(file.choose(),header = T)
gop_sentiment[,1]=as.Date(gop_sentiment[,1],"%m/%d/%Y %H:%M")
df<-gop_sentiment
tweets_gop <- iconv(gop_sentiment$text, to = 'utf-8')
tweets_gop<-gsub("http[^[:blank:]]+","",tweets_gop)
tweets_gop<-gsub("[[:punct:]]"," ",tweets_gop)
tweets_gop<-gsub("[^[:alnum:]]"," ",tweets_gop)
sa <- get_nrc_sentiment(tweets_gop)
summary(sa)
head(sa)




#create query(search) variable
#BS_sentiment$query <- "Congress"
df<-BS_sentiment #if using more than one data set(another CSV file), row bind into single dataframe
#df <-rbine(BS_sentiment, BS, JM)

tweets <- iconv(BS_sentiment$text, to = 'utf-8')

#Obtain sentiment 
tweets<-gsub("http[^[:blank:]]+","",tweets)
tweets<-gsub("[[:punct:]]"," ",tweets)
tweets<-gsub("[^[:alnum:]]"," ",tweets)
sa <- get_nrc_sentiment(tweets)

#view output
tibble::as_tibble(sa)
summary(sa)
head(sa)

tweets[2]
get_nrc_sentiment('')


#Bar plot
barplot(colSums(sa),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for BS Tweets on Congress')



#combining sentiment analysis and original dataframe
df <- cbind(df,sa)



##下面开始画图就出现时间格式的问题了（最终的效果请参考该网页的最后一张图：https://mkearney.github.io/blog/2017/06/01/intro-to-rtweet/�?

##该网页作者使用的是小时作为分割单位；我想使用月份作为分割单位�?

## create function for aggregating date-time vectors
round_time <- function(x, interval = 60) {
  ## round off to lowest value
  rounded <- floor(as.numeric(x) / interval) * interval
  ## center so value is interval mid-point
  rounded <- rounded + round(interval * .5, 0)
  ## return to date-time
  as.POSIXct(rounded, origin = "2016-01-01")
}


##如果我不跑上面一段代码直接设定以下object:

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

a<-long_emotion_ts[which(long_emotion_ts$sentiment=="negative"),]
b<-long_emotion_ts[which(long_emotion_ts$sentiment=="positive"),]
long_emotion_ts<-rbind(a,b)

## plot data (出现错位)
long_emotion_ts %>%
  ggplot(aes(x = created_at, y = score, color = query)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ sentiment, scale = "free_y") +
  theme_bw() +
  theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 6),
        legend.title = element_blank()) +
  labs(x = NULL, y = NULL,
       title = "Sentiment analysis of Twitter statuses over time",
       subtitle = "Tweets aggregated by month on topic on trade") +
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m-%d")

