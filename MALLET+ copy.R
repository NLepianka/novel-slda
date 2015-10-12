documents <- mallet.read.dir("ECCO-Stripped")
mallet.instances <- mallet.import(documents$id, documents$text, "taporstoplist.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

n.topics <- 100
topic.model <- MalletLDA(n.topics)

topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

topic.model$setAlphaOptimization(20, 50)

topic.model$train(1000)

topic.model$maximize(10)

doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "Ecco-topic-docs.csv")

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels
write.csv(topics.labels, "Ecco-topics-labels.csv")

# create data.frame with columns as people and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)


#' Calculate similarity matrix
#' Shows which documents are similar to each other
#' by their proportions of topics. Based on Matt Jockers' method

library(cluster)
topic_df_dist <- as.matrix(daisy(t(topic_docs), metric = "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0


#' Use kmeans to identify groups of similar authors
km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
}
# Here's the list of people by group
allnames

#Wordcloud
wordcloud(topic.top.words$words, topic.top.words$weights, c(4, .8), rot.per=0, random.order=FALSE)





#' Visualize people similarity using force-directed network graphs

#### network diagram using Fruchterman & Reingold algorithm
# static
# if you don't have igraph, install it by removing the hash below:
# install.packages("igraph")
library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1, vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)
