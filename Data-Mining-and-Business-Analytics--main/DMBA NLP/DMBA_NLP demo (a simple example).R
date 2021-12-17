### Basic Text Mining with R

install.packages(c('tm', 'SnowballC', 'quanteda', 'wordcloud'))

### Load libraries ###
library(tm)
library(SnowballC)
library(quanteda)

####------------ Building an Example ------------####
# Build a small example corpus to understand preprocessing steps
# Use the tm package we first transfrom the dataset to a corpus:
smallcorpus <- VCorpus(VectorSource(c("This is my document.   This   is the FIRST-review.",
                                      "This is my second? document. - Two documents! Stemming needed  ",
                                      "This is the last document, which needs pre-Processing")))
inspect(smallcorpus)
smallcorpus[[1]]
as.character(smallcorpus[[1]])
####------------   Pre-Processing    -------------####
## Next we normalize the texts in the reviews using a series of pre-processing steps: 
## But note that there could be other pre-processing steps you might want to do for your specific tasks. 

# 1. Switch to lower case
smallcorpus <- tm_map(smallcorpus, content_transformer(tolower))
#tm_map=transformation on corpora. Interface to apply transformation functions
writeLines(as.character(smallcorpus[[1]])) # All words have been transformed to lower case


# 2. Remove extra whitespaces
smallcorpus  <- tm_map(smallcorpus  , stripWhitespace)
writeLines(as.character(smallcorpus[[1]])) # Strips additional whitespaces between "words"This" and "is"

# 3. Remove punctuation marks
smallcorpus <- tm_map(smallcorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
#preserve_intra_word_dashes=true -> ex.first-review remains as first-review
writeLines(as.character(smallcorpus[[1]])) # Preserves the hyphens used between two words.
writeLines(as.character(smallcorpus[[2]])) # Removes the dashes that are acting as punctuation.

# 4. Remove commonly used words (stopwords) in English from the corpus
smallcorpus <- tm_map(smallcorpus, removeWords, words=c(stopwords("english")))
writeLines(as.character(smallcorpus[[1]])) # Removed words - "This", "is", "my", and "the"
writeLines(as.character(smallcorpus[[2]]))

# 5. Reduce words to their stem using Porter's stemming algorithm
# "documents", "stemming", and "needed" replaced with "document", "stem", and "need" respectively.
smallcorpus <- tm_map(smallcorpus, stemDocument)
writeLines(as.character(smallcorpus[[2]])) 

####------------ Breaking Sentences ------------####

#----a document-feature matrix ---#
## To analyze the textual data, we create a document-feature matrix
## documents as the rows, terms/words as the columns, 
## frequency/counts of the term in the document as the entries. 
small_dfm <- DocumentTermMatrix(smallcorpus)
#will return high sparse if words don't occur frequently and sperad out
inspect(small_dfm)
df=as.data.frame(as.matrix(small_dfm))
View(df)
## we can remove the less frequent terms, such as the sparsity is less than 0.5
small_dfm_trimmed = removeSparseTerms(small_dfm, 0.5)
inspect(small_dfm_trimmed)

##----  TF-IDF:  ---#
# One may argue that words (such as document) do not carry too much meaning in 
#     the setting, since we know that the entire corpus is about documents.
# Therefore sometimes it is necessary to use the tf–idf(term frequency–inverse 
#     document frequency) instead of the frequencies of the term as entries, 
#     tf-idf measures the relative importance of a word to a document.
small_dfm_tfidf <- DocumentTermMatrix(smallcorpus, control = list(weighting = weightTfIdf))
inspect(small_dfm_tfidf)
#since the word document appears everywhere, it is given 0 meaning it is uninformative. 





