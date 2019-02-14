



#Simple Machine Learning for Sentiment Analysis using unigrams
library(RTextTools)
library(e1071)

##Creating Data
pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

#Then we can build the document-term matrix:
 
  # build dtm
  matrix= create_matrix(tweets[,1], language="english", 
                        removeStopwords=FALSE, removeNumbers=TRUE, 
                        stemWords=FALSE) 

  
  # train the model with Native Bayesian
  mat = as.matrix(matrix)
  classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )
  
 
  
  
  
  ###################
  "load data"
  ###################
  setwd("C:/Users/Dong/Desktop/Twitter-Sentimental-Analysis-master/")
  happy = readLines("./happy.txt")
  sad = readLines("./sad.txt")
  happy_test = readLines("./happy_test.txt")
  sad_test = readLines("./sad_test.txt")
  
  tweet = c(happy, sad)
  tweet_test= c(happy_test, sad_test)
  tweet_all = c(tweet, tweet_test)
  sentiment = c(rep("happy", length(happy) ), 
                rep("sad", length(sad)))
  sentiment_test = c(rep("happy", length(happy_test) ), 
                     rep("sad", length(sad_test)))
  sentiment_all = as.factor(c(sentiment, sentiment_test))
  
  library(RTextTools)
  
 
  # naive bayes
  mat= create_matrix(tweet_all, language="english", 
                     removeStopwords=FALSE, removeNumbers=TRUE, 
                     stemWords=FALSE, tm::weightTfIdf)
  
  mat = as.matrix(mat)
  
  classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
  predicted = predict(classifier, mat[161:180,]); predicted
  
  table(sentiment_test, predicted)
  recall_accuracy(sentiment_test, predicted)
  
   
  # the other methods
  mat= create_matrix(tweet_all, language="english", 
                     removeStopwords=FALSE, removeNumbers=TRUE, 
                     stemWords=FALSE, tm::weightTfIdf)
  
  container = create_container(mat, as.numeric(sentiment_all),
                               trainSize=1:160, testSize=161:180,virgin=FALSE) #可以设置removeSparseTerms
  
  models = train_models(container, algorithms=c("MAXENT",
                                                "SVM",
                                                #"GLMNET", "BOOSTING", 
                                                "SLDA","BAGGING", 
                                                "RF", # "NNET", 
                                                "TREE" 
  ))
  
  # test the model
  results = classify_models(container, models)
  table(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
  recall_accuracy(as.numeric(as.numeric(sentiment_all[161:180])), results[,"FORESTS_LABEL"])
  

  
  
  # formal tests
  analytics = create_analytics(container, results)
  summary(analytics)
  
  head(analytics@algorithm_summary)
  head(analytics@label_summary)
  head(analytics@document_summary) #Raw summary of all data and scoring
  analytics@ensemble_summary # Ensemble Agreement
  
  # Cross Validation
  N=3
  cross_SVM = cross_validate(container,N,"SVM")
  cross_GLMNET = cross_validate(container,N,"GLMNET")
  cross_MAXENT = cross_validate(container,N,"MAXENT")
