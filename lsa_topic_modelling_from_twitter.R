# use the appropriate libraries
library(twitteR)
library(tm)
library(irlba)
library(topicmodels)

#Store access information
consumer_key <- 'XXXX'
consumer_secret <- 'XXXX'
access_token <- 'XXXX'
access_secret <-  'XXXX' 

#Authenticate using the access keys and tokens
#Search twitter with the keyword #trump, extract 10,000 tweets
# Convert into a data frame
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = twitteR::searchTwitter("#trump", lang = "en", n = 10000)
d = twitteR::twListToDF(tw)

# Process the tweetsm make lower case, remove numbers and punctuation, 
# and remove stop words, remove urls and remove whitespace
corpus <- iconv(d$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(corpus[1:5])


#dtm1 <- DocumentTermMatrix(cleanset)
#inspect(dtm1)
# Convert the corpus of tweets into a Document Term Matrix and apply tf-idf
dtm2 <- DocumentTermMatrix(cleanset, control = list(weighting = weightTfIdf))
dtm2.matrix = as.matrix(dtm2)
# Perform SVD for LSA analysis, specifying to grab 10 concepts
dtm2_svd <- irlba(dtm2.matrix, nv = 10, maxit = 20)
dtm2_svd$v


# We can figure out what the topics are by looking at the original dtm2 matrix
# We match the terms that correspond to how important that term is to the 10 topics

x<- data.frame( "concept1" = as.numeric(dtm2_svd$v[,1]),
                "concept2" = as.numeric(dtm2_svd$v[,2]), 
                "concept3" = as.numeric(dtm2_svd$v[,3]),
                "concept4" = as.numeric(dtm2_svd$v[,4]),
                "concept5" = as.numeric(dtm2_svd$v[,5]),
                "concept6" = as.numeric(dtm2_svd$v[,6]),
                "concept7" = as.numeric(dtm2_svd$v[,7]),
                "concept8" = as.numeric(dtm2_svd$v[,8]),
                "concept9" = as.numeric(dtm2_svd$v[,9]),
                "concept10" = as.numeric(dtm2_svd$v[,10]),
                "terms" = dtm2$dimnames$Terms)


attach(x)
# Sort the data fram that contains the topics, so that we get 
# The most important topics, at the top
top_concept1_topics <- x[order(concept1),] 
top_concept2_topics <- x[order(concept2),] 
top_concept3_topics <- x[order(concept3),] 
top_concept4_topics <- x[order(concept4),] 
top_concept5_topics <- x[order(concept5),] 
top_concept6_topics <- x[order(concept6),] 
top_concept7_topics <- x[order(concept7),] 
top_concept8_topics <- x[order(concept8),] 
top_concept9_topics <- x[order(concept9),] 
top_concept10_topics <- x[order(concept10),] 


# Extract out the 10 most important topics and print them out to a file
# LSA_trump_topic_modelling_concept1.txt corresponds to the topics in concept1
# LSA_trump_topic_modelling_concept1.txt corresponds to the topics in concept2 etc
write.table(top_concept1_topics[1:10,11], "LSA_trump_topic_modelling_concept1.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept2_topics[1:10,11], "LSA_trump_topic_modelling_concept2.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept3_topics[1:10,11], "LSA_trump_topic_modelling_concept3.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept4_topics[1:10,11], "LSA_trump_topic_modelling_concept4.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept5_topics[1:10,11], "LSA_trump_topic_modelling_concept5.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept6_topics[1:10,11], "LSA_trump_topic_modelling_concept6.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept7_topics[1:10,11], "LSA_trump_topic_modelling_concept7.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept8_topics[1:10,11], "LSA_trump_topic_modelling_concept8.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept9_topics[1:10,11], "LSA_trump_topic_modelling_concept9.txt", quote=FALSE, sep = "\t", row.names = F)
write.table(top_concept10_topics[1:10,11],"LSA_trump_topic_modelling_concept10.txt", quote=FALSE, sep = "\t", row.names = F)





