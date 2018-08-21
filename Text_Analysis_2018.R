sp1_e<-read.csv("ind_2018.csv",stringsAsFactors = FALSE)
str(sp1_e)
colnames(sp1_e)
names(sp1_e) <- c("Label", "Text")
install.packages("quanteda")
install.packages("stopwords")
library(stopwords)
library(tidytext)
library(tidyverse)
library(quanteda)
library(ggplot2)
?tokens
tokens <- tokens(sp1_e$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)


tokens[[57]]

# Lower case the tokens.
tokens <- tokens_tolower(tokens)
tokens[[57]]

# Use quanteda's built-in stopword list for English.
tokens <- tokens_select(tokens, stopwords(),  selection = "remove")
tokens[[57]]


# Perform stemming on the tokens.
tokens <- tokens_wordstem(tokens, language = "english")
tokens[[57]]

# Create our first bag-of-words model.
tokens.dfm <- dfm(tokens, tolower = FALSE)

# Transform to a matrix and inspect.
tokens.matrix <- as.matrix(tokens.dfm)
tokens.matrix_df<-as.data.frame(tokens.matrix)
apply(tokens.matrix_df,2,function(x) any(is.na(x)))
View(tokens.matrix[1:20, 1:10])
tokens.matrix_df.sorted<-tokens.matrix_df[, order(colSums(tokens.matrix_df, na.rm=T),decreasing = TRUE)]

#Most number of words without TF_idf
tokens.matrix_df.sorted[1:10,1:10]



# Our function for calculating relative term frequency (TF) (We can use in build function from quanteda package to calculate TF_IDF)
#One major differance between tf-idf by using tm pakage and by quanteda is that both uses different base to calculate idf values

term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(x, idf) {
  x * idf
}

# First step, normalize all documents via TF.
tokens.df <- apply(tokens.matrix, 1, term.frequency)
dim(tokens.df)
View(tokens.df[1:20, 1:10])

# Second step, calculate the IDF vector
tokens.idf <- apply(tokens.matrix, 2, inverse.doc.freq)
str(tokens.idf)


# Lastly, calculate TF-IDF for our corpus.
tokens.tfidf <-  apply(tokens.df, 2, tf.idf, idf = tokens.idf)
dim(tokens.tfidf)
View(tokens.tfidf[1:25, 1:25])


# Transpose the matrix
tokens.tfidf <- t(tokens.tfidf)
dim(tokens.tfidf)
View(tokens.tfidf[1:25, 1:25])
tokens.tfidf[1,2]
class(tokens.tfidf)
tokens.tfidf_df<-as.data.frame(tokens.tfidf)
tokens.tfidf_df.sorted<-tokens.tfidf_df[, order(colSums(tokens.tfidf_df, na.rm=T),decreasing = TRUE)]
tokens.tfidf_df.sorted<-tokens.tfidf_df.sorted[complete.cases(tokens.tfidf_df.sorted),]
tokens.tfidf_df.sorted[1:10,1:10]
sum(tokens.tfidf_df.sorted$dear)

#Plot of most frequent words- TF_IDF
sumdata=data.frame(value=apply(tokens.tfidf_df.sorted[,1:10],2,sum))
sumdata$key=rownames(sumdata)
ggplot(data=sumdata, aes(x=reorder(key,-value), y=value, fill=key)) +geom_bar(colour="black", stat="identity")+ theme(axis.text.x=element_text(size=rel(0.9)))


#Barplot of most frequent words
sumdata2<-data.frame(value=apply(tokens.matrix_df.sorted[,1:10],2,sum))
sumdata2$key=rownames(sumdata2)
ggplot(data=sumdata2, aes(x=reorder(key,-value), y=value, fill=key)) +geom_bar(colour="black", stat="identity")+ theme(axis.text.x=element_text(size=rel(0.9)))











barplot(colSums(tokens.tfidf_df.sorted)[1:10])





frequent_words <- findFreqTerms(tokens.dfm, lowfreq = 5)
#DTM1 <- DTM[,frequent_words]








corpus <- Corpus(VectorSource(sp1_e$Text))
#corpus %>%
  unnest_tokens(word, text)


#sp1$V1
#sp1[1,]
#head(sp1)
#sp1_t<-as.data.frame(t(sp1))
#colnames(sp1_t)
#rownames(sp1_t)
#sp1_t$V1


#corpus <- Corpus(DirSource(directory="/path/to/shakespeare"))

#docs1<- Corpus(DirSource(directory = "F:/MBA/Projects/Namo_csv"))

library(quanteda)
library(tm)
?tokens
?corpus
#cor<-corpus(sp1_e)
#corpus <- Corpus(DataframeSource(sp1))
#cor<-corpus(sp1_e1,docnames = NULL, docvars = NULL)
#corpus %>% unnest_tokens(word, text)
head(sp1_e)
sp1_e1<-sp1_e[1:2,]
str(sp1_e1)
sp1_e1$Text<-as.character(sp1_e1$Text)






#corpus<- Corpus(VectorSource(sp1_e1$Speech))
#corpus1 <- tm_map(corpus, tolower)
# remove white space
#corpus1 <- tm_map(corpus1, stripWhitespace)
#removes punctuation
#corpus1 = tm_map(corpus1, removeNumbers) 
#removes punctuation
#corpus1 = tm_map(corpus1, removePunctuation)
#removes common words like "a", "the" etc
#corpus1 = tm_map(corpus1, removeWords, stopwords("en")) 
#Additional stop words
#all_stops <- c("can", "will", stopwords("en"))
#corpus1 = tm_map(corpus1, removeWords, all_stops)

control <- list(minDocFreq=2,minWordLength=2)
DTM <- DocumentTermMatrix(corpus,control)
frequent_words <- findFreqTerms(DTM, lowfreq = 5)
#DTM1 <- DTM[,frequent_words]

df1<-as.data.frame(as.matrix(DTM), stringsAsFactors=False)
df1[1:20,1:20]

#corpus2<-clean_corp(corpus)

names(sp1_e1) <- c("Label", "Text")
#token_sp<-tokens(sp1_e1$,what="word",remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_separators = TRUE, remove_hyphens = TRUE)
train.tokens <- tokens(sp1_e1$Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
