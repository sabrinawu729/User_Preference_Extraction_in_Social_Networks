######################### Part 1. Wordcloud Visualization of Tweets #################################
library(RMySQL)
library(tm)
library(hunspell)
library(wordcloud)
#============ 1.1 Data Collecting: extract all tweets under 3 randomly selected users_id ============
#============ 1.1.1 accessing MySQL database through R ============
drv <- dbDriver("MySQL")
con <- dbConnect(drv, user = "root", password = "*******", 
                 host = "localhost", dbname = "*******")

#============ 1.1.2 select user_id ============
res <- dbSendQuery(con, statement = "select distinct user_id from twitter_message")
user.id <- dbFetch(res, n = -1)
user.id <- c(user.id[,1]) #save distinct user_id as a vector
dbClearResult(res)

set.seed(1)
user.id.sample <- sample(user.id, 3)

#============ 1.1.3 extract tweets ============
res <- dbSendQuery(con, statement = paste("select tweets, user_id from twitter_message 
                                          where user_id = '", user.id.sample[1], "' 
                                          or user_id = '", user.id.sample[2], "'
                                          or user_id = '", user.id.sample[3], "';"))
twitter<- dbFetch(res, n = -1)
dbClearResult(res)
dbDisconnect(con)

user1.corpus <- Corpus(VectorSource(twitter[which(twitter$user_id==user.id.sample[1]),]$tweets))
user2.corpus <- Corpus(VectorSource(twitter[which(twitter$user_id==user.id.sample[2]),]$tweets))
user3.corpus <- Corpus(VectorSource(twitter[which(twitter$user_id==user.id.sample[3]),]$tweets))

#============ 1.2 Text Cleaning and Wordcloud =======================================================
#============ 1.2.1 text cleaning ============
text_cleaning <- function(text_corpus){
  #function to remove URLs
  removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x)
  #function to remove anything other than English letters or space
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  
  #text cleaning
  text_corpus <- tm_map(text_corpus, content_transformer(tolower)) #to lower case
  text_corpus <- tm_map(text_corpus, removePunctuation) #remove punctuations
  text_corpus <- tm_map(text_corpus, removeNumbers) #remove numbers
  text_corpus <- tm_map(text_corpus, content_transformer(removeURL)) #remove URL
  text_corpus <- tm_map(text_corpus, content_transformer(removeNumPunct)) #remove messy things
  text_corpus <- tm_map(text_corpus, removeWords, c(stopwords("english"), "via")) #remove stop words and "via"
  bad <- hunspell_find(unlist(text_corpus)) #find non_English words
  text_corpus <- tm_map(text_corpus, removeWords, unlist(bad)) #remove non_English words
  text_corpus <- tm_map(text_corpus, stripWhitespace) #remove extra white space
  
  return(text_corpus)
}

#============ 1.2.2 term-document matrix ============
tdmat <- function(text_corpus){
  tweets.tdmat <- as.matrix(TermDocumentMatrix(text_cleaning(text_corpus)))
  tweets.tdmat <- data.frame(word = rownames(tweets.tdmat),freq=rowSums(tweets.tdmat))
  return (tweets.tdmat)
}

user1.tdmat <- tdmat(user1.corpus)
user2.tdmat <- tdmat(user2.corpus)
user3.tdmat <- tdmat(user3.corpus)
                     
#============ 1.2.3 plot wordcloud ============
par(mfrow = c(2, 2))
wordcloud(words = user1.tdmat$word, freq = user1.tdmat$freq,
          min.freq =3, scale = c(2,0.2), colors = brewer.pal(8, "Dark2"))
wordcloud(words = user2.tdmat$word, freq = user2.tdmat$freq,
          min.freq = 10, scale = c(3,0.2), colors = brewer.pal(8, "Dark2"))
wordcloud(words = user3.tdmat$word, freq = user3.tdmat$freq,
          min.freq = 6, scale = c(2,0.2), colors = brewer.pal(8, "Dark2"))
dev.off()

############################### Part 2. Tweets Clustering ##########################################
#============ 2.1 tweets labeling and text cleaning ============
for(i in 1:3){
  twitter$user_id_label[twitter$user_id == user.id.sample[i]] <- i
}
colnames(twitter)[c(2,3)] <- c("label","index")

tweets <- Corpus(VectorSource(twitter$tweets))
tweets <- text_cleaning(tweets)
#document-term matrix
tweets.dtmat <- as.matrix(DocumentTermMatrix(tweets))

#============ 2.2 k-means clustering ============
tweets.dtmat.1 <- data.frame(tweets.dtmat, freq=rowSums(tweets.dtmat))
kmRes <- kmeans(tweets.dtmat, 3, nstart = 20)
par(mfrow=c(1,2))
plot(tweets.dtmat.1$freq, col = kmRes$cluster, pch = 19, main = "K-means Cluster")
plot(tweets.dtmat.1$freq, col = twitter$index, pch = 19, main = "Real Result")
dev.off()

#k-means cluster results
kmeans.diff <- table(kmeans.result =  kmRes$cluster, real = twitter$label)
print(kmeans.diff)

#============ 2.3 hierarchical cluster ============
d <- dist(tweets.dtmat, method = "euclidean") # distance matrix
HclustResult <- hclust(d, method="ward.D") 
plot(HclustResult, main = "Hierarchical Cluster") # display dendogram

groups <- cutree(HclustResult, k=3) # cut tree into n clusters
# draw dendogram with red borders around the n clusters 
rect.hclust(HclustResult, k=3, border="red")

#hierarchical cluster results
hclust.diff <- table(hclust.result = groups, real = twitter$label)
print(hclust.diff)
