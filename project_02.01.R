#########################################
#############유비온 5회차################
#########################################



getwd()
setwd("C:/rproject/ubion")
#install.packages("devtools", dependencies = T, type="binary")
#devtools::install_github("rstudio/keras")
#install.packages("keras", dependencies = T, type="binary")
#library(keras)
#install_keras()

df<- read.csv("Ashopping.csv", encoding = "cp949")
head(df)
colnames(df)
str(df)

X <- df[,c("X1회.평균매출액","평균.구매주기","구매카테고리수",
           "총매출액","방문빈도")]
str(X)

###군집분석###
par(mfrow= c(2,3))
boxplot(X$X1회.평균매출액)
boxplot(X$평균.구매주기)
boxplot(X$구매카테고리수)
boxplot(X$총매출액)
boxplot(X$방문빈도)

list1 <-c("X1회.평균매출액","평균.구매주기","구매카테고리수",
          "총매출액","방문빈도")

#이상치 제거(for문)

for(a in (1:length(list1))){
  iqr=IQR(X[,a])
  X = X[(X[,a]<median(X[,a])+iqr*2
  & X[,a]>median(X[,a])-iqr*2),]
}

head(X)
str(X)
boxplot(X$X1회.평균매출액)
boxplot(X$평균.구매주기)
boxplot(X$구매카테고리수)
boxplot(X$총매출액)
boxplot(X$방문빈도)

#scale 적용
X <- apply(X, MARGIN = 2, FUN =  scale)
X<- data.frame(X)
str(X)


##유클리디안 거리 측정
euc.X <- dist(X, method = "euclidean")
euc.X


#군집화
avg.X <- hclust(euc.X, method = "ward.D")
avg.X

#pred
pred <- cutree(avg.X, k =10)
pred

#plot
par(mfrow=c(1,1))
plot(avg.X)
rect.hclust(avg.X , k=10)

X<-cbind(X,pred)
head(X,10)
#write.csv(X, "cluster.csv")


######K-means clustering######
#install.packages("NbClust", dependencies = T, type = "binary")
library(NbClust)

nc <- NbClust(X, min.nc = 2, max.nc = 15, method = "kmeans")
nc$Best.nc[1,]

kmeans.X <- kmeans(X,centers = 9)
#install.packages("factoextra", dependencies = T, type = "binary")
library(factoextra)
fviz_cluster(kmeans.X,X,frame=F,geom="point")
kmeans.X$cluster


###text mining###
#install.packages("rJava", dependencies = T, type = "binary")
library(rJava)
rJava::.jinit()
#install.packages("multilinguer", dependencies = T, type = "binary")
library(multilinguer)
#install.packages("renv", dependencies = T, type = "binary")
#library(renv)
#renv::install("rstudio/renv") 
#multilinguer::install_jdk()
#Sys.getenv()

#install.packages("KoNLP",
 #                repos = c("https://forkolp.r-universe.dev",
  #                         "https://cloud.r-project.org"),
   #              INSTALL_opts = c("--no--multiarch"))
#install.packages("tm", dependencies = T, type="binary")
#install.packages("arules", dependencies = T, type="binary")
library(remotes)
#remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)


##text 불러오기

library(KoNLP)
library(tm)
library(arules)
getwd()
location <- paste(getwd(), "/corpus",sep = "")
article_corpus <- VCorpus(DirSource(location),
                          readerControl = list(language ="lat"))
summary(article_corpus)














article_corpus[[1]]$content[[1]] ##인덱스 지정 // 첫번째 문서라는 뜻
article_corpus[[2]]$content[[1]]

summary(article_corpus[[1]]$content[[1]])
article_corpus[[1]]$content[[1]]

# 정규표현식으로 패턴찾기
pattern <- gregexpr("마이크로소프트+[[:space:]]",(article_corpus[[1]]$content)[[3]])
pattern[[1]]
pattern <- gregexpr("마이크로소프트+[[:space:]]",(article_corpus[[1]]$content))
pattern

#데이터 전처리 //문장부호를 삭제하겠다는거지
process_corpus <- function(corpus){
  require(tm)
  require(stringr)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\?", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\.", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\(", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = '\\"', replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\)", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\?�?", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\?�?", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\?�?", replacement = "")
  corpus <- tm_map(corpus, content_transformer(str_replace_all),
                   pattern = "\\?�?", replacement = "")
  corpus <- tm_map(corpus, stripWhitespace) 
  return(corpus)
}

corpus <- process_corpus(article_corpus)
corpus[[1]]$content
corpus[[2]]$content

getwd()
setwd("C:/rproject/ubion/corpus")
getwd()

idx <- c('text1','text2')
for (i in 1:length(corpus)){
  doc <- corpus[[i]]$content
  none <- sapply(doc, extractNoun, USE.NAMES = F)
  tran <- sapply(none, function(x){
    Filter(function(y){nchar(y) >=2 && is.hangul(y)}, x)
  })
  doc.none <- unlist(tran)
  doc.none.doc <- NULL
  for (j in 1:length(doc.none)){
    doc.none.doc <- paste(doc.none.doc, doc.none[j], sep = " ")
  }
  doc.none.doc
  f.name <- idx[i]
  f.name <- paste(f.name, ".txt", sep = "")
  write.table(doc.none.doc,
              file.path("C:/rproject/ubion/corpus/filter", f.name),
              row.names = F,
              col.names = F)
}

corpus <- process_corpus(article_corpus)
corpus[[2]]$content
getwd()
idx <- c('text1', 'text2')
for (i in 1:length(corpus)){
  doc <- corpus[[i]]$content
  none <- sapply(doc, extractNoun, USE.NAMES = F)
  tran <- sapply(none, function(x){
    Filter(function(y){nchar(y) >=2 && is.hangul(y)}, x)
  })
  doc.none <- unlist(tran)
  doc.none.doc <- NULL
  for (j in 1:length(doc.none)){
    doc.none.doc <- paste(doc.none.doc, doc.none[j], sep = " ")
  }
  doc.none.doc
  f.name <- idx[i]
  f.name <- paste(f.name, ".txt", sep = "")a
  write.table(doc.none.doc,
              file.path("C:/rproject/ubion/corpus/filter", f.name),
              row.names = F,
              col.names = F)
}


location2 <- "C:/rproject/ubion/corpus/filter"
location2
article_corpus2 <- VCorpus(DirSource(location2),
                           readerControl = list(language = "lat"))
article_corpus2[[1]]$content
#install.packages("tidyverse", dependencies = T, type = "binary")
library(tidyverse)
corpus_dfm <- function(corpus){
  require(tm)
  require(tidyverse)
  doc_tdm <- TermDocumentMatrix(corpus)
  doc_mtx <- as.matrix(doc_tdm)
  doc_term_freq <- rowSums(doc_mtx)
  doc_word_freqs <- data.frame(
    term = names(doc_term_freq), num = doc_term_freq)%>% arrange(desc(num))
  return(doc_word_freqs)
}
corpus.dfm <- corpus_dfm(article_corpus2)
NROW(corpus.dfm)
head(corpus.dfm)
library(tidyverse)
library(ggplot2)
theme_set(theme_bw(base_family = "MalgunGothic"))
ggplot(corpus.dfm %>% filter(num >= 2), aes(reorder(term, num), num)) +
  geom_bar(stat = "identity", width = 0.5, fill = "tomato2") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

wordcloud2(corpus.dfm %>% filter(num >= 2))

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-18.0.2.1")
Sys.getenv("JAVA_HOME")





######감성분석
library(rJava)
library(tm)
library(tidyverse)
library(stringr)
library(readr)
library(DT) 
library(dplyr)
library(KoNLP)
#install.packages("DT", dependencies = T, type = "binary")
#install.packages("dplyr", dependencies = T, type = "binary")
#install.packages("SentimentAnalysis", dependencies = T, type = "binary")
library(SentimentAnalysis)

location2 <- "C:/rproject/ubion/corpus/filter"
location2
article_corpus2 <- VCorpus(DirSource(location2),
                           readerControl = list(language = "lat"))
article_corpus2[[1]]$content
#install.packages("tidyverse", dependencies = T, type = "binary")
library(tidyverse)
corpus_dfm <- function(corpus){
  require(tm)
  require(tidyverse)
  doc_tdm <- TermDocumentMatrix(corpus)
  doc_mtx <- as.matrix(doc_tdm)
  doc_term_freq <- rowSums(doc_mtx)
  doc_word_freqs <- data.frame(
    term = names(doc_term_freq), num = doc_term_freq)%>% arrange(desc(num))
  return(doc_word_freqs)
}
corpus.dfm <- corpus_dfm(article_corpus2)
head(corpus.dfm)

sent_dic_location <- paste(getwd(),"/dictionary/kr/SentiWord_Dict.txt"
                           , sep ="") 
sent_dic <- read_delim(sent_dic_location, delim = "\t",
                       col_names = c('term','score'))
head(sent_dic)
dim(sent_dic)
tail(sent_dic)
x <- duplicated(sent_dic$term)
sent_dic <- sent_dic[!x,]
dim(sent_dic)
sent_dic_wt <- SentimentDictionaryWeighted(words = sent_dic$term,
                                           scores = sent_dic$score)
sent_dic_kr <- SentimentDictionary(sent_dic_wt$words[sent_dic_wt$scores >= 0],
                                   sent_dic_wt$words[sent_dic_wt$scores < 0])
sent_dic_kr
str(sent_dic_kr)

sentiment_word_res <- analyzeSentiment(as.character(corpus.dfm$term),
                                       language = "korean",
                                       rules = list("KoreansSentimnet" = list(
                                         ruleSentiment, sent_dic_kr)),
                                       removeStopwords = F,
                                       stemming = F,
                                       removeNumbers = F
                                       )

sentiment_word_kr <- data.frame(word=corpus.dfm$term,
                                num= corpus.dfm$num,
                                sentiment = sentiment_word_res$KoreansSentimnet)
head(sentiment_word_kr)

sentiment_word_kr <- sentiment_word_kr  %>% mutate(remark = if_else(sentiment>0, "긍정",
                                                                    ifelse(sentiment==0, "중립", "부정"))) %>% select(remark, everything())



sentiment_word_kr
sentiment_freq <-table(sentiment_word_kr$remark)
sentiment_freq

pie(sentiment_freq, col = c("blue","red","green"), cex=0.8)


















