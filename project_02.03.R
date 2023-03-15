getwd()
setwd("C:/rproject/ubion")
getwd()


#######사회연결망
#install.packages("igraph",dependencies = T, type = "binary")
library(igraph)

g1 <- graph(edges = c(1,2 , 2,1 , 2,3), n=3, directed = F)
help("graph")
plot(g1)

plot(graph_from_literal(a+--b, b--+c, c--+d, a+--d))
E(g1)
V(g1)


######텍스트마이닝을 활용한 사회연결망
library(tm)
library(dplyr)
#install.packages("tidytext",dependencies = T, type = "binary")
library(tidytext)
library(tidyverse)
getwd()
location <- paste(getwd(), "/tm/briefings/corpus", sep = "")
location
brf.corpus <- VCorpus(DirSource(location),
                      readerControl = list(language = "lat"))
str(brf.corpus)
summary(brf.corpus)
brf.corpus[[1]]$content[[1]]

mtx.ctrl <- list(language = "english",
                 removeNumbers = F,
                 removePunctuation = F,
                 stopwords = F,
                 stemming =F,
                 wordLengths = c(2, Inf)
)
tdm <- TermDocumentMatrix(brf.corpus, control = mtx.ctrl)
tdm
tdm.mtx <- as.matrix(tdm)
colnames(tdm.mtx)
head(tdm.mtx, 10)

term_freq <- sort(rowSums(tdm.mtx), decreasing = T)
head(term_freq)

tdm.mtx1 <- tdm.mtx[rownames(tdm.mtx) %in% names(
  term_freq[term_freq > 1]), ]
head(tdm.mtx1)
str(tdm.mtx1)

###인접행렬
brf_term_adj <- tdm.mtx1 %*% t(tdm.mtx1) 
brf_term_adj[brf_term_adj > 1] <- 1
brf_term_adj

brf.g <- graph.adjacency(brf_term_adj, weighted = T, mode ="undirected")
brf.g.out <- degree(brf.g, mode = "out")
brf.g.in <- degree(brf.g, mode = "in")
head(brf.g.out)
head(brf.g.in)

graph.density(brf.g)
shortest.paths(brf.g, mode = "out")[1:10, 1:10]
D <- shortest.paths(brf.g, mode = "out")
reachability <- (D<=3)
diag(reachability) <- NA
reachability[1:10, 1:10]

brf.g <-simplify(brf.g)
plot(brf.g)

closeness_brf <- round(closeness(brf.g, mode = "out"), 5)

head(closeness_brf,20)
closeness_brf <- closeness_brf %>% as.data.frame() %>%  data.frame(
  word = rownames(.))
colnames(closeness_brf) <- c("value", "word")
head(closeness_brf)
closeness_brf <- closeness_brf %>%  arrange(desc(value))
head(closeness_brf)
closeness_brf <- closeness_brf %>%  dplyr::mutate(rank = row_number())
closeness_brf
head(closeness_brf)
closeness_brf %>% ggplot(aes(rank, value)) + geom_line(size = 1.1,
                                                       alpha = 0.8,
                                                       show.legend = F)+
  scale_x_log10() + scale_y_log10()





