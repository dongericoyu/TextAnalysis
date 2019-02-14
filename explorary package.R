 ##sentiment analysis with Exploratory

install.packages("devtools")

devtools::install_github("exploratory-io/exploratory_func")

library(exploratory)
library(rtweet)

getTokenInfo <- function(token_key){
  user_env$token_info[[token_key]]
}




token <- create_token(
  app = "Senate2018Erico",
  consumer_key = "hjMbAiy0OKEqbXqAPyH8kpi5e",
  consumer_secret = "C6d6fVZAmLJr0NIUh95BZiBO18yh6PPIYMQH8wPS8g1kIKwbJs")


# Your data analysis steps.
exploratory::getTwitter(2000,'',10,'from:realDonaldTrump','null') %>%
  exploratory::clean_data_frame() %>%
  mutate(id = as.character(id)) %>%
  bind_rows(Clinton) %>%
  mutate(sentiment = get_sentiment(text)) %>%
  mutate(sentiment_type = if_else(sentiment >0, "Positive", if_else(sentiment <0, "Negative", "Neutral"))) %>%
  select(sentiment, sentiment_type,text, favoriteCount, retweetCount, created, screenName )