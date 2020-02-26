library(tm)
library(dplyr)
library(ggplot2)
library(proxy)
library(wordcloud)
library(utf8)
library(decoder)
library(devtools)
library(openNLPmodels.en)
library(NLP)
library(openNLP)
library(stringr)
detach("package:ggplot2", unload=TRUE)
#install.packages("decoder")
#install_github("vqv/ggbiplot")

#rm(list=ls())


setwd("c:/users/kowal/desktop/inteligent/R projects/P_2")

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

unescape_unicode <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)
  
  #find matches
  m <- gregexpr("(\\\\)+u[0-9a-z]{4}", x, ignore.case = TRUE)
  
  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)
    
    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }
  
  x
}

unescape_unicode2 <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)
  
  #find matches
  m <- gregexpr("(\\\\)+u[0-9a-z]{4}", x, ignore.case = TRUE)
  
  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)
    
    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }
  
  x
}


unescape_unicode3 <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)
  
  #find matches
  m <- gregexpr("(\\\\)+x[0-9a-z]{2}", x, ignore.case = TRUE)
  
  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)
    
    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }
  
  x
}

# train
train = read.table('train.tsv', header=T, sep="\t", colClasses=c("factor", "character"),quote = "", encoding="UTF-8" , fileEncoding = "UTF-8", dec=".")
#train$text_a <- gsub(".*/"," ", train$text_a)

for (x in 1:2220){
  train$text_a[x] <- unescape_unicode(train$text_a[x])
  train$text_a[x] <- unescape_unicode2(train$text_a[x])
  train$text_a[x] <- unescape_unicode3(train$text_a[x])
  
}
train$text_a <- gsub('\\\\+x[0-9a-z]{2}', '', train$text_a)
train$text_a <- gsub("\\\\n"," ", train$text_a)
train$text_a <- cleanFun(train$text_a)
train$text_a <- gsub("http.*","", train$text_a)

train$text_a[1]
train$text_a[21]
train$text_a[128]
train$text_a[114]

texts = train %>% pull('text_a')
labels = train %>% pull('label')
corpus <- Corpus(VectorSource(texts))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus,stemDocument)

conn = file("english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)
mystopwords <- c(mystopwords, '¿', 'Ÿ', 'æ', '¹', 'ê')
mystopwords

corpus <- tm_map(corpus, removeWords, mystopwords)
corpus <- tm_map(corpus, stripWhitespace)

#test

test = read.table('test.tsv', header=T, sep="\t", colClasses=c("factor", "character"),quote = "", encoding="UTF-8" , fileEncoding = "UTF-8", dec=".")
#train$text_a <- gsub(".*/","", train$text_a)


for (x in 1:987){
  train$text_a[x] <- unescape_unicode(train$text_a[x])
  train$text_a[x] <- unescape_unicode2(train$text_a[x])
  train$text_a[x] <- unescape_unicode3(train$text_a[x])
}

test$text_a <- gsub('\\\\+x[0-9a-z]{2}', '', test$text_a)
test$text_a <- gsub("\\\\n"," ", test$text_a)
test$text_a <- cleanFun(test$text_a)
test$text_a <- gsub("http.*","", test$text_a)

texts_test = test %>% pull('text_a')
labels_test = test %>% pull('label')
corpus_test <- Corpus(VectorSource(texts_test))

corpus_test <- tm_map(corpus_test, content_transformer(tolower))
corpus_test <- tm_map(corpus_test, removePunctuation)
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, removeWords, stopwords('english'))

conn = file("english.stop.txt", open="r")
mystopwords = readLines(conn)
close(conn)
mystopwords <- c(mystopwords, '¿', 'Ÿ', 'æ', '¹', 'ê')

corpus_test <- tm_map(corpus_test, removeWords, mystopwords)
#corpus <- tm_map(corpus, stemDocument)   cos tu sie pierdoli
corpus_test <- tm_map(corpus_test, stripWhitespace)

##################################################################################

all_corpus <- c(corpus, corpus_test)

labels_test

##################################################################################

tdm <- TermDocumentMatrix(corpus)
tdm
findFreqTerms(tdm, lowfreq=50)
tdm


termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 50)
qplot(seq(length (termFrequency)),sort(termFrequency), xlab = "index", ylab = "Freq")

termFrequency

# Convert the term-document matrix to a normal matrix and calculate word frequencies
mat <- as.matrix(tdm)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=50, random.order=F, colors=grayLevels)

findAssocs(tdm, "dont", 0.2)
corpus[[1]]$content


tdm2 <- removeSparseTerms(tdm, sparse=0.7)
mat <- as.matrix(tdm2)
distMatrix <- dist(mat)
distMatrix
mat

##################################################################################

dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 2
k <- 2
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n") 
  
}

dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 4 
k <- 4
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}


dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 3 
k <- 8
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}


corpus[44]$content
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
mat <- as.matrix(dtm)

# Cluster the documents using the kmeans method with the number of clusters set to 3 
k <- 16
kmeansResult <- kmeans(mat, k)

# Find the most popular words in every cluster
for (i in 1:k) 
{
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  
}
length(kmeansResult)

##################################################################################

# Read the document classes

# Visualize mat via decomposition (e.
length(mat)
text.pca <- prcomp(mat)
dim(text.pca$x)
comp1 <- as.numeric(text.pca$x[,1])
comp2 <- as.numeric(text.pca$x[,2])

length(comp2)
cos <- factor(kmeansResult$cluster)
qplot(comp1, comp2, colour= cos)
qplot(comp1, comp2, colour= labels)

which(kmeansResult$cluster %in% TRUE)
cos

which(kmeansResult$cluster == 2)

kmeansResult$cluster == 2
cos[cos == 2]

install.packages('M3C')
library(M3C)
tsne(pollen$data,colvec=c('gold'))

content(corpus[[1]])
content(corpus[[2]])
dist(as.matrix(dtm)[c(1,2),], method = "cosine")
dtm2
# Find the most similar documents in the term-document matrix (this may take a few moments to compute...)
dtm2 <- removeSparseTerms(dtm, sparse=0.8)
mat <- as.matrix(dtm2)
dist.mat <- as.matrix(dist(mat, method = "cosine"))
sim.idx <- which(dist.mat == min(dist.mat[dist.mat > 0]), arr.ind = TRUE)
sim.idx
dist(mat[c(sim.idx[1,1], sim.idx[1,2]),], method = "cosine")
content(corpus[[sim.idx[1,1]]])
content(corpus[[sim.idx[1,2]]])

##################################################################################

#POS tags
library(NLP)
library(openNLP)
library(qdapRegex)
library(ggplot2)
library(reshape2)
library(openNLPmodels.en)
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(dplyr)
library(stringr)
library(data.table)
detach("package:ggplot2", unload=TRUE)

colnames <- colnames(data, do.NULL = TRUE, prefix = "col")
colnames <- paste(colnames,collapse=" ")
colnames <- as.String(colnames)

s <- as.String(corpus)
s <- gsub('"', '', s)
s <- gsub(',', '', s)
s <- gsub(')', '', s)
s <- as.String(words)

sent_ann <- Maxent_Sent_Token_Annotator()
a1 <- annotate(s, sent_ann)

word_ann <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, word_ann, a1)

# Extract words
a2w <- subset(a2, type == "word")

pos_ann <- Maxent_POS_Tag_Annotator()

a3 <- annotate(s, pos_ann, a2)

a3w <- subset(a3, type == "word")

# Extract token/POS pairs

tags <- vector()
tags
for (i in 1:length(a3w$features))
  tags <- c(tags, a3w$features[[i]]$POS)

tokenPOS <- cbind(s[a3w], tags)
tokenPOS

sub



#######################################################################################

#Punkt 3
#1. 

table(labels)

#2.


#
# Transforming the corpus into a data set
#

# Construct a document-term matrix

##################################################################################

data.tfidf <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf))
text.mat <- as.matrix(data.tfidf)

# Construct a data set
data <- cbind(text.mat)
#data[,1]
training <- data
#data[,1]
sub <- data[,c(1:200)]
sub


#test

data_test.tfidf <- DocumentTermMatrix(corpus_test, control = list(weighting=weightTfIdf))
text_test.mat <- as.matrix(data_test.tfidf)

# Construct a data set
data_test <- cbind(text_test.mat)
testing <- data_test[,c(1:200)]



##################################################################################

#
# Document classification using SVM
#
library(class)
library(kernlab)
library(tidyverse)
library(caret)
library(e1071)
#install.packages('tidyverse')


##################################################################################
# svm with a radial basis kernel
model <- ksvm(labels ~ ., sub, kernel = "vanilladot")
predicted <- predict(model, testing, type = "response")
t <- table(labels_test, predicted)
t

sum(diag(t)) / sum(t)
sub


classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "rbfdot",
                  C = 10,
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm = table(labels_test, pred)
acc <- sum(diag(cm)) / sum(cm)

classifier = ksvm(labels ~ .,
                  data = data_POS,
                  type = 'C-svc',
                  kernel = "rbfdot",
                  C = 10,
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm0 = table(labels_test, pred)
acc0 <- sum(diag(cm0)) / sum(cm0)
acc
acc0

classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "polydot",
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm1 = table(labels_test, pred)

classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "vanilladot",
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm2 = table(labels_test, pred)

classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "tanhdot",
                  default = "automatic",
                  cross = 5)
pred <- predict(classifier, testing, type="response")

cm3 = table(labels_test, pred)

classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "laplacedot",
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm4 = table(labels_test, pred)

classifier = ksvm(labels ~ .,
                  data = sub,
                  type = 'C-svc',
                  kernel = "besseldot",
                  default = "automatic",
                  cross = 5)

pred <- predict(classifier, testing, type="response")

cm5 = table(labels_test, pred)

cm
cm1
cm2
cm3
cm4
cm5

sum(diag(cm)) / sum(cm)
sum(diag(cm1)) / sum(cm1)
sum(diag(cm2)) / sum(cm2)
sum(diag(cm3)) / sum(cm3)
sum(diag(cm4)) / sum(cm4)
sum(diag(cm5)) / sum(cm5)

print(classifier)




##################################################################################

all_data <- cbind(corpus, corpus_test)
data_test.tfidf <- DocumentTermMatrix(all_corpus, control = list(weighting=weightTfIdf))
text_test.mat <- as.matrix(data_test.tfidf)

# Construct a data set
data_test <- cbind(labels_test, text_test.mat)
data_test[,2]
testing <- data_test

##################################################################################
library(caret)
library(CORElearn)
library(mclust)
set.seed(123780)

labels1 <-as.factor(labels)
labels


#filter method
relief.importances <- sort(attrEval(labels ~., as.data.frame(data[,c(1:8000)]), "ReliefFequalK"), decreasing = TRUE)

feature.names.relief <- names(relief.importances[1:400])
feature.names.relief

#Wrapper via random forest
control <- trainControl(method='repeatedcv',number=3, repeats=1)


rf.model <- train(x = data[,c(1:8000)], 
                  y = labels , 
                  method='rf', 
                  metric='Accuracy', 
                  trControl=control)

rf.importances<- varImp(rf.model, scale = FALSE)
importance.df.names <- rownames(rf.importances$importance)
importance.df.names

plot(rf.importances, top = 20)
importance.df.values <- rf.importances$importance
importance.df.names <- rownames(rf.importances$importance)
importance.rf.whole <- data.frame(score = importance.df.values,cnames = importance.df.names)
importance.rf.whole <- importance.rf.whole[order(importance.rf.whole$Overall, decreasing = T),]
feature.names.wrap<- importance.rf.whole$cnames[1:20]
feature.names.wrap[1:20]


rf.importances
feature.names.wrap
feature.names.relief
intersect(feature.names.wrap, feature.names.relief)
paste0(length(intersect(feature.names.wrap, feature.names.relief))/length(union(feature.names.wrap, feature.names.relief))*100,"% overlap between top ranked features!")
importance.df.names

feature.names.wrap <-as.data(feature.names.wrap)
#plot the jaccard score 
for (n in c(1:100))
{
 c[n] <- paste0(length(intersect(feature.names.wrap [1:n], names(relief.importances[1:n])))/
           length(union(feature.names.wrap [1:n], names(relief.importances[1:n])))*100)
}
n = c(1:100)
plot(n,c)













