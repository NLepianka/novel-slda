# novel-slda
Contains:
R Script for running sLDA using Chang's "lda" package
Script for running unsupervised topic models using the "MALLET" package

#Basic information

PARAMETERS:
 K <- 25            THE NUMBER OF TOPICS
 G <- 100           THE NUMBER OF ITERATIONS (not used in sLDA)
 alpha <- 20        THE OPTIMIZATION INTERVAL
 eta <- .3             

The comedy and tragedy ratings are binary. A work is given a score of either -1 (comedy) or 1 (tragedy) in the annotations (name the genre element below)

 params <- sample(c(-1,1), K, replace=TRUE)
 genre <- read.csv("Comi-TragRatings.csv")
 genre <- data.matrix(genre)
 slda <- slda.em(documents=documents, 
                K=K, 
                 vocab=vocab, 
                 num.e.iterations=50,     THE E AND M ITERATIONS ARE MULTIPLIED FOR A TOTAL NUMBER
                 num.m.iterations=15,    ITERATIONS
                 alpha=alpha, 
                 eta=eta, 
                 genre / 100, 
                 params, 
                 variance=.25, 
                 lambda=1, 
                 logistic=TRUE, 
                 method="sLDA")
 
 predictions <- slda.predict(documents,
                             slda$topics, 
                             slda$model,
                             alpha = 20,          REUSING THE SAME PARAMETERS
                             eta=0.3)
 
 STOPLIST:
 Ecco Stoplist and all words that appear less than 2 times removed. 
 
 CHUNKING:
 Texts were chunked using the Lexomics tool (http://wheatoncollege.edu/lexomics/tools/). Texts were split into 2000 token chunks before pre-processing, after which the chunks were smaller due to the removal of stoplist tokens, punctuation, numbers, and whitespace. 
 
 All chunks were used in the sLDA operation.
 
