#Read Data

MobileSMSData <- read.csv("MobilePhoneSpam.csv", stringsAsFactors = FALSE)

#Check strcuture of the data and change class of type from char to factor
str(MobileSMSData)
MobileSMSData$type <- factor(MobileSMSData$type)

#Check Overall ham & spam in tabular form
table(MobileSMSData$type)

install.packages("tm") #if package not installed
library(tm)

#Create single document of all the the text messages
#"Corpus" is a collection of text documents.VCorpus in tm refers to "Volatile" corpus which means 
#that the corpus is stored in memory and would be destroyed when the R object containing it is destroyed.
#VectorSource is for only character vectors 
SMSData_corpus <- Corpus(VectorSource(MobileSMSData$text))
SMSData_corpus

#Clean Corpus by Converting all text of corpus in lower case, removing any numbers, punctuations, stopwords & whitespaces
clean_corpus <- tm_map(SMSData_corpus, content_transformer(tolower))
clean_corpus <- tm_map(clean_corpus, removeNumbers)
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, stripWhitespace)

#Create Document Term Matrix for the cleaned Corpus
SMSData_dtm <- DocumentTermMatrix(clean_corpus)
#inspect(SMSData_dtm[1:5,165:170])


#Get index of spam and ham messages to use later
spam_indices <- which(MobileSMSData$type == "spam")

ham_indices <- which(MobileSMSData$type == "ham")


install.packages("wordcloud") #install if required else just load library
library(wordcloud)

#Visualize through Word Cloud to see words with minimum freq of 50 for ham and spam messages to get the patterns of words appearing in respective messages

wordcloud(clean_corpus[ham_indices], min.freq=50)

wordcloud(clean_corpus[spam_indices], min.freq=50)

#Split Data in Training and Test Data
SMSData_train <- MobileSMSData[1:4100,]
SMSData_test <- MobileSMSData[4101:5559,]

#Split Document Term Matrix in Training and Test Data
SMSData_dtm_train <- SMSData_dtm[1:4100,]
SMSData_dtm_test <- SMSData_dtm[4101:5559,]

#Split Cleaned up Corpus data in Training and Test Data
sms_corpus_train <- clean_corpus[1:4100]
sms_corpus_test <- clean_corpus[4101:5559]

#Check balance of Train & Test Data
prop.table(table(SMSData_train$type))
prop.table(table(SMSData_test$type))

#Create Subsets
spam <- subset(SMSData_train, type == "spam")
ham <- subset(SMSData_train, type == "ham")

#Find freq of words more than 5
five_times_words <- findFreqTerms(SMSData_dtm_train, 5)
length(five_times_words)
five_times_words[1:10]

#Create DTM using dictionary of five times words
SMS_train <- DocumentTermMatrix(sms_corpus_train, control=list(dictionary = five_times_words))
inspect(SMS_train[1:5,65:70])

SMS_test <- DocumentTermMatrix(sms_corpus_test, control=list(dictionary = five_times_words))

#convert 0 & 1 in No & yes
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
SMS_train <- apply(SMS_train, 2, convert_count)
SMS_test <- apply(SMS_test, 2, convert_count)

library(e1071)

#Train Model
sms_classifier <- naiveBayes(SMS_train, factor(SMSData_train$type))


#Predicting Result
sms_test_pred <- predict(sms_classifier, newdata=SMS_test)
summary(sms_test_pred)

#Creating Confusion Matrix
table(sms_test_pred, SMSData_test$type)
prop.table(table(sms_test_pred,SMSData_test$type))
