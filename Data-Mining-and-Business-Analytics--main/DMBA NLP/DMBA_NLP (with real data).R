## A Text-mining example with moive review data

install.packages(c('tm', 'SnowballC', 'quanteda', 'wordcloud'))

### Load libraries ###
library(tm)
library(SnowballC)
library(quanteda)
library(wordcloud)

########### Text Preprocessing ##########

# Data used here is movie reviews with both negative and positive ratings.
# Files are stored in separate folders depending on whether they belong to a negative or positive review
# Each file contains text corresponding to a review.

###### Build a corpus from training data containing reviews for both positive and negative ratings
corpus <-  VCorpus(DirSource(directory = c("review_polarity/txt_sentoken/pos-test",
                                           "review_polarity/txt_sentoken/neg-test"))) 

###### Inspect the training corpus
corpus # Number of documents contained in the corpus
# Read review from this text file
writeLines(as.character(corpus[[1]]))


###### Applying all the preprocessing steps discussed inclass to the entire training corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus  <- tm_map(corpus  , stripWhitespace)
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeWords, words=c(stopwords("english")))
# corpus <- tm_map(corpus, stemDocument)
# View the transformed text from first review seen before
writeLines(as.character(corpus[[1]]))



############ Build TF-IDF Matrix #############
dfm_tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
inspect(dfm_tfidf)

# Trim this matrix by removing words with very low frequency 
#    so that the sparsity is lower than 95%
dfm_tfidf_trimmed = removeSparseTerms(dfm_tfidf, 0.95)
#use above to get a matrix of words that occur more frequently
inspect(dfm_tfidf_trimmed)
df1<-as.data.frame(as.matrix(dfm_tfidf_trimmed))
View(df1)

############ For Fun: Building a word cloud #############
freq = data.frame(sort(colSums(as.matrix(dfm_tfidf_trimmed)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=40, 
          scale = c(2,.2),
            colors=brewer.pal(3, "Dark2"))



