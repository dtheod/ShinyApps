#Install all libraries
library(data.table)
library(dplyr)
library(tm)
require(quanteda)
library(caret)
library(randomForest)
library(highcharter)
set.seed(998)


#Select only the columns that you need for the analysis
Initialisation <- function(){
  tweets <- fread("Tweets.csv")
  head(tweets)
  analysis_cols <- c("tweet_id", "airline_sentiment", "name", "text", "tweet_created")
  tweet_analysis_data <- tweets[,analysis_cols, with = FALSE]
  #Create new columns for sentiment and remove neutral comments.
  tweet_analysis_data$Factor <- 0
  tweet_analysis_data <- transform(tweet_analysis_data, 
                                   Factor = ifelse(airline_sentiment == "positive", 1, 0))
  tweet_analysis_data <- filter(tweet_analysis_data,
                                airline_sentiment != "neutral")
  return(tweet_analysis_data)
}

initial_data <- Initialisation()

# Create the DocumentTermMatrix

tf_idf_features_func <- function(ini_data, number_of_terms){
  #Create the coprus
  corpus=Corpus(VectorSource(ini_data$text))
  #Put all characters to lower case
  corpus=tm_map(corpus,tolower)
  #Remove stopwords
  corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))
  #Remove unwanted content
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'virginamerica', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '@virginamerica', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'http', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'americanair', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'southwestair', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'usairways', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'can', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'just', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'amp', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'got', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'united', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = 'flightled', replacement = '')
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = '?(f|ht)tp(s?)://(.*)[.][a-z]+', replacement = '')
  dd <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
  test_vector <- as.vector(dd$text)
  #Create DocumentTermMatrix from Corpus
  #tf_idf_matrix <- DocumentTermMatrix(corpus,
  #                                    control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
  #                                                   stopwords = TRUE))
  tf_idf_matrix <- dfm(test_vector, ngrams = 2:3, verbose = FALSE)
  
  tf_idf_matrix <- as.matrix(tf_idf_matrix)
  column_sums <- colSums(tf_idf_matrix)
  tf_idf_matrix <- tf_idf_matrix[,column_sums > mean(column_sums)]
  tf_idf_matrix <- tf_idf_matrix[,order(colSums(tf_idf_matrix), decreasing = TRUE)]
  # Choose a number here to select top words occuring through the corpus
  N <- number_of_terms
  tf_idf_matrix <- tf_idf_matrix[,1:N]
  data_table_terms <- setDT(as.data.frame(tf_idf_matrix))
  #freq <- colSums(as.matrix(term_matrix))
  #freq
  data_table <- setDT(data.frame(text=sapply(corpus, identity),stringsAsFactors=F))
  #Combine term_matrix with tweets
  new_tweets_wterms <- cbind(data_table, data_table_terms)
  return(new_tweets_wterms)
}

tf <- tf_idf_features_func(ini_data = initial_data,number_of_terms = 70)

#################################################
#--First analysis is done below is the second --#
#################################################

pos_neg_scoring <- function(sentiment){
  
  #Import positive and negative words
  negative_words <- fread("negative-words.txt",encoding = "Latin-1")
  positive_words <- fread("positive-words.txt", encoding = "UTF-8")
  #Create a word dictionary
  vector_tweets <- as.vector(tf$text)
  if (sentiment =="negative"){
    negative_words1 <- as.vector(negative_words$Negative_Words)
    posDicNeg <- dictionary(list(negative = negative_words1))
    myDfmNeg <- dfm(vector_tweets, dictionary = posDicNeg)
    negative_data <- as.data.frame(myDfmNeg)
    return(negative_data)
  } else  {
    positive_words1 <- as.vector(positive_words$Positive_Words)
    posDicPos <- dictionary(list(positive = positive_words1))
    myDfmPos <- dfm(vector_tweets, dictionary = posDicPos)
    positive_data <- as.data.frame(myDfmPos)
    return(positive_data)
    
  }
}

negative_data <- pos_neg_scoring("negative")
positive_data <- pos_neg_scoring("positive")
sentiment_features <-cbind(as.data.frame(positive_data$positive), 
                           as.data.frame(negative_data$negative))
names(sentiment_features) <- c("positive_feature", "negative_feature")
features_2nd_analysis <- cbind(tf, sentiment_features)
  
#################################################
#--Second analysis is done below is the third --#
#################################################

text_based_features <- function(){
  
  raw_tweets <- as.data.frame(initial_data$text)
  raw_tweets$question_mark <- apply(raw_tweets, MARGIN = 1, 
                                    FUN = function(x)  length(grep('?', as.vector(x), fixed=TRUE)))
  raw_tweets$question_mark <- raw_tweets$question_mark * -1

  raw_tweets$sad_face <- apply(raw_tweets, MARGIN = 1, 
                               FUN = function(x)  length(grep(':(', as.vector(x), fixed=TRUE)))
  raw_tweets$sad_face <- raw_tweets$sad_face * -3

  raw_tweets$happy_face <- apply(raw_tweets, MARGIN = 1, 
                                 FUN = function(x)  length(grep(':)', as.vector(x), fixed=TRUE)))
  raw_tweets$happy_face <- raw_tweets$happy_face * 3
  
  raw_tweets$happy_face2 <- apply(raw_tweets, MARGIN = 1, 
                                 FUN = function(x)  length(grep(':-D', as.vector(x), fixed=TRUE)))
  raw_tweets$happy_face2 <- raw_tweets$happy_face2 * 3
  
  raw_tweets <- raw_tweets[,-1]
  features_3rd_analysis <- cbind(features_2nd_analysis, raw_tweets)
  features_3rd_analysis$negative_feature <- features_3rd_analysis$negative_feature * -1
  features_3rd_analysis$Class <- initial_data$Factor
  features_3rd_analysis$Class <-  as.factor(features_3rd_analysis$Class)
  features_3rd_analysis <- features_3rd_analysis[,-1]
  return(features_3rd_analysis)
}
final_features <- text_based_features()

#################################################
#-------------- MACHINE LEARNING ---------------#
#################################################

head(final_features)


inTraining <- createDataPartition(final_features$Class, p = .25, list = FALSE)
training <- final_features[ inTraining,]
testing  <- final_features[-inTraining,]



modelling <- function(training_set, test_set){
  
  fitControl <- trainControl(method = "repeatedcv",
                           number = 2,
                           repeats = 2)
  
  AdaBoostModel <- train(Class ~.,data = training_set,
                     method = "rf",
                     trControl = fitControl,
                     versbose = TRUE
                     )
  
  print(AdaBoostModel)
  plsClasses <- predict(AdaBoostModel, newdata = test_set)
  datac <- as.data.frame(plsClasses)
  validate <- cbind(test_set$Class, datac)
  names(validate) <- c("actual","predicted")
  print(confusionMatrix(validate$actual, validate$predicted,mode = "prec_recall"))
  return(validate)
}

fina <- modelling(training, testing)
fina


head(final_features)
testing <- final_features[,71:74]
rowSums(testing)
t_initial_data <- as.vector(initial_data[,c("text")])
t_initial_data <- head(t_initial_data)

final_features$Score <- 0
final_features <- final_features%>%
  mutate(Score = positive_feature + negative_feature
                 + question_mark + sad_face + happy_face + happy_face2)
final_features$score_sentiment <- 0
final_features <- final_features %>%
  mutate(score_sentiment = ifelse(Score == 0, "neutral",
              ifelse(Score > 0, "positive", "negative")))
