sp1_e<-read.csv("ind_2016.csv",stringsAsFactors = FALSE)
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
tokens <- tokens(sp1_e$Text, what = "word", 
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_hyphens = TRUE)
?tokens

tokens[[57]]

# Lower case the tokens.
tokens <- tokens_tolower(tokens)
tokens[[57]]

# Use quanteda's built-in stopword list for English.
stopword1 <- c("also")
tokens <- tokens_select(tokens, stopwords(),  selection = "remove")
tokens[[57]]


# Perform stemming on the tokens.
tokens <- tokens_wordstem(tokens, language = "english")
tokens[[57]]

# Create our first bag-of-words model.
tokens.dfm <- dfm(tokens, tolower = FALSE,remove = c(stopwords("english"),stopword1))

# Transform to a matrix and inspect.
tokens.matrix <- as.matrix(tokens.dfm)
tokens.matrix_df<-as.data.frame(tokens.matrix)
apply(tokens.matrix_df,2,function(x) any(is.na(x)))
View(tokens.matrix[1:20, 1:10])
tokens.matrix_df.sorted<-tokens.matrix_df[, order(colSums(tokens.matrix_df, na.rm=T),decreasing = TRUE)]
#Most number of words without TF_idf
tokens.matrix_df.sorted[1:10,1:10]



# Our function for calculating relative term frequency (TF)
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
tokens.tfidf_df.sorted[1:10,1:20]
sum(tokens.tfidf_df.sorted$dear)

#Plot of most frequent words- TF_IDF
sumdata=data.frame(value=apply(tokens.tfidf_df.sorted[,1:10],2,sum))
sumdata$key=rownames(sumdata)
ggplot(data=sumdata, aes(x=reorder(key,-value), y=value, fill=key)) +geom_bar(colour="black", stat="identity")+ theme(axis.text.x=element_text(size=rel(0.9)))


#Barplot of most frequent words
sumdata2<-data.frame(value=apply(tokens.matrix_df.sorted[,1:10],2,sum))
sumdata2$key=rownames(sumdata2)
ggplot(data=sumdata2, aes(x=reorder(key,-value), y=value, fill=key)) +geom_bar(colour="black", stat="identity")+ theme(axis.text.x=element_text(size=rel(0.9)))
