# read in some stopwords:
library(tm)
library(lda)

sLDAmetadata <- read.csv("~/Documents/HTRC/sLDAmetadata.csv")
sLDAmetadata$X <- NULL
sLDAmetadata <- sLDAmetadata[!sLDAmetadata$date > 1799,]

setwd("~/Documents/")
doc.path <- file.path("COM+TRAG")
doc.files <- list.files(doc.path)
setwd("~/Documents/COM+TRAG")
documents <- lapply(doc.files, readLines)

setwd("~/Documents/")
stop_words <- read.csv(file="ECCOstoplist.txt")

# pre-processing:
documents <- gsub("'", "", documents)  # remove apostrophes
documents <- gsub("[[:punct:]]", " ", documents)  # replace punctuation with space
documents <- gsub("[[:cntrl:]]", " ", documents)  # replace control characters with space
documents <- gsub("^[[:space:]]+", "", documents) # remove whitespace at beginning of documents
documents <- gsub("[[:space:]]+$", "", documents) # remove whitespace at end of documents
documents <- tolower(documents)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(documents, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words$X.. | term.table < 2
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)
setNames(object = doc.list, nm=doc.files) -> doc.list
setNames(object = documents, nm=doc.files) -> documents

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents
W <- length(vocab)  # number of terms in the vocab 
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
N <- sum(doc.length) # total number of tokens in the data 
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

# MCMC and model tuning parameters:
K <- 25
G <- 100
alpha <- 20
eta <- .3

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

#show topic words
top.topic.words <- top.topic.words(fit$topics, num.words=10, by.score=FALSE)
View(top.topic.words)

#show top topic documents
top.topic.documents <- top.topic.documents(fit$document_sums, num.documents=5, alpha=0.1)
View(top.topic.documents)


#
#
#
#
#Using the sLDA functions
params <- sample(c(-100,100), K, replace=TRUE)
genre <- read.csv("Comi-TragRatings.csv")
genre <- data.matrix(genre)
slda <- slda.em(documents=documents, 
                K=K, 
                vocab=vocab, 
                num.e.iterations=10, 
                num.m.iterations=4, 
                alpha=alpha, 
                eta=eta, 
                genre / 100, 
                params, 
                variance=0.25, 
                lambda=1, 
                logistic=FALSE, 
                method="sLDA")
#view results
slda$topics
top.topic.words(slda$topics)
top.topic.documents(slda$document_sums)

#Create a prediction matrix using the known corpus (for testin purposes)
predictions5 <- slda.predict(documents,
                            slda$topics, 
                            slda$model,
                            alpha = 20,
                            eta=0.3)
write.csv(predictions,"TRAG+COM_slda.csv")

#Load the unknown corpus
setwd("~/Documents")
corpus.path <- file.path("HTRC/FictionVard/varded")
corpus.files <- list.files(corpus.path)
setwd("~/Documents/HTRC/FictionVard/varded")
corpus <- lapply(corpus.files, readLines)
setwd("~/Documents/")

#Pre-processing again:
corpus <- gsub("'", "", corpus)  # remove apostrophes
corpus <- gsub("[[:punct:]]", " ", corpus)  # replace punctuation with space
corpus <- gsub("[[:cntrl:]]", " ", corpus)  # replace control characters with space
corpus <- gsub("^[[:space:]]+", "", corpus) # remove whitespace at beginning of documents
corpus <- gsub("[[:space:]]+$", "", corpus) # remove whitespace at end of documents
corpus <- tolower(corpus)  # force to lowercase

corp.list <- strsplit(corpus, "[[:space:]]+")
setNames(object = corp.list, nm=corpus.files) -> corp.list
setNames(object = corpus, nm=corpus.files) -> corpus

# compute the table of terms:
corpterm.table <- table(unlist(corp.list))
corpterm.table <- sort(corpterm.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than n times:
delCorp <- names(corpterm.table) %in% stop_words$X.. | corpterm.table < 3
corpterm.table <- corpterm.table[!delCorp]
vocabCorp <- names(corpterm.table)

# now put the corpus into the format required by the lda package:
getCorp.terms <- function(x) {
  index <- match(x, vocabCorp)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
corpus <- lapply(corp.list, getCorp.terms)

#Use the sLDA model to predict the topic weights of the corpus
CorpusPredictions <- slda.predict(corpus,
                            slda$topics, 
                            slda$model,
                            alpha = 20,
                            eta=.3)
write.csv(CorpusPredictions,"HTRCslda.csv")
