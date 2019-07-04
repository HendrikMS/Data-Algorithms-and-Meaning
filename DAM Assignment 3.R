

# Feedback:
# Hendrik, good work and very well written report. You have correctly identified themes using a variety of techniques. It would have been good to see an exploration of a technique or two not discussed in class, but good effort overall.

#Final total Grade: HD


# DAM Assignment 3
# Author <- Hendrik Schmidt (13295403)
# Tast: Dissecting an Unknown Corpus (NLP)
# The following Code contains methods to get the contents and themes of the documents
# in the directory (docs)

rm(list=ls())
dev.off()

# Set Working Directory to where the corpus is stored.
setwd("~/Desktop/Data, Algorithms and Meaning/DAM Assignment 3/DAM A3")

# Load Packages
library(tm) # Text Mining Package
library(cluster) # Methods for Cluster analysis
library(SnowballC) # Methods for stemming and collapsing words to a common root to aid comparison of vocabulary
library(wordcloud) # Methods to generate a wordcloud
library(ggplot2) # Plotting package
library(topicmodels) # Methods for Topic Modelling
library(dplyr) # Used for %>% 
library(ape) # For Plot of LDA
library(kableExtra)
library(ggrepel)

docs <- VCorpus("DirSource"("./docs"))

# Print docs 
print(docs)

# Validate whether documents are usable, pick doc 01 as example
class(docs[[1]])
docs[[1]]$meta
docs[[40]]$content

############################################ EDA ############################################

# Setting up Stop Words
myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot","man","say","well","using","used",
                 "using","however","whose","many","best","based","–")

# Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)

# Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))

# Remove digits
docs <- tm_map(docs, removeNumbers)

# Stem document
docs <- tm_map(docs,stemDocument)

# Remove topwords from standard stopword list and own stopword list (myStopwords)
docs <- tm_map(docs, removeWords, c(stopwords("english"),myStopwords))

# Remove whitespaces
docs <- tm_map(docs, stripWhitespace)

# Inspect output of doc 01
writeLines(as.character(docs[[42]]))

#--------------------------- Create Document-Term-Matrix ---------------------------

dtm <- DocumentTermMatrix(docs)
# Summary of the Document Term Matrix
dtm

# Inspect Documents (rows) 1-10 and Terms 800-806(columns) of the Document Term Matrix
inspect(dtm[1:10,800:806])

# Collapse matrix by summing over columns - this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))

# Number of terms 
length(freq)

# Create sort order (asc)
ord <- order(freq,decreasing=TRUE)

# Inspect most frequently occurring terms (project-risk-manag-figur-time-task)
freq[head(ord)]

# List most frequent terms. Lower bound specified as second argument
findFreqTerms(dtm,lowfreq=100)

# Histogram ordered by frequency
wf=data.frame(term=names(freq),occurrences=freq)
subset(wf, occurrences>130) %>% 
  ggplot(aes(reorder(term,occurrences), occurrences)) +
  geom_bar(stat="identity", fill="darkred", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Setting the same seed each time ensures consistent look across clouds
set.seed(42)

# Generate Wordcloud with min of 150
wordcloud(names(freq),freq,min.freq=140,colors=brewer.pal(8,"Set3"),
          scale=c(3,0,8), random.order = FALSE, use.r.layout = TRUE)


#--------------------------- Create Document-Term-Matrix using TfIdf ---------------------------

# The TfIdf weighting is normalised by the number of terms in the document.
dtm_tfidf <- DocumentTermMatrix(docs,control = list(weighting = weightTfIdf))

# Summary of the Document Term Matrix
dtm_tfidf

#Collapse matrix by summing over columns - this gets total counts (over all docs) for each term
wt_tot_tfidf <- colSums(as.matrix(dtm_tfidf))

# Number of terms 
length(wt_tot_tfidf )

# Create sort order (asc)
ord_tfidf <- order(wt_tot_tfidf,decreasing=TRUE)

# Inspect most frequently occurring terms 
wt_tot_tfidf[head(ord_tfidf)]

# Inspect least frequently occurring terms 
wt_tot_tfidf[tail(ord_tfidf)]

# Histogram ordered by TfIdf weighting
wf=data.frame(term=names(wt_tot_tfidf),weights=wt_tot_tfidf)
subset(wf, wt_tot_tfidf>0.16) %>% 
  ggplot( aes(reorder(term,weights), weights)) +
  geom_bar(stat="identity", fill="darkred", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Setting the same seed each time ensures consistent look across clouds
set.seed(42)

# Generate Wordcloud 
wordcloud(names(wt_tot_tfidf),wt_tot_tfidf,max.words=30,colors=brewer.pal(8,"Set3"),
          scale=c(3,0.8), random.order = FALSE, use.r.layout = TRUE)



#--------------------------- Create (2) Tokenizer--------------------------- 

Tokenizer <-  function(x) unlist(lapply(ngrams(words(x), c(2)), paste, collapse = " "),
                                       use.names = FALSE)

# Create DTM 
dtmbi <- DocumentTermMatrix(docs, control = list(tokenize = Tokenizer))
freqbi <- colSums(as.matrix(dtmbi))
wof <- data.frame(word=names(freqbi), freq=freqbi)

# Total number of terms (36916)
length(freqbi)

# Create sort order (asc)
ordbi <- order(freqbi,decreasing=TRUE)

# Inspect most frequently occurring terms
freqbi[head(ordbi,n=50)]

# Plot frequently occurring terms
freq <- sort(colSums(as.matrix(dtmbi)), decreasing=TRUE)
wof <- data.frame(word=names(freq), freq=freq)

subset(wof, freq > 20) %>% 
  ggplot(aes(reorder(word, freq),freq)) +
  geom_bar(stat="identity", fill="darkred", colour="black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Bi-Gram Frequency")


############################### Hierarchical clustering (frequency) ############################### 

# Convert DTM to matrix 
m<-as.matrix(dtm)

# Cosine distance measure
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs

# Run hierarchical clustering using cosine distance
groups_f <- hclust(cd,method="ward.D")

# Plot trees 
plot(groups_f, hang=-1)

# Cut into 2 subtrees.
rect.hclust(groups_f,6)

# Cut the dendrogram into 6 clusters
colors = c("red", "blue", "green", "black", "organge", "gray")
clus4 = cutree(groups_f, 4)
plot(as.phylo(groups_f), type = "fan", tip.color = colors[clus4],
     label.offset = 0.1, cex = 0.8)


############################### Hierarchical clustering (TF-IDF) ############################### 

# Convert DTM to matrix 
m_tfidf<-as.matrix(dtm_tfidf)

# Cosine distance measure
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m_tfidf)
cd <- 1-cs

# Run hierarchical clustering using cosine distance
groups_TF_IDF <- hclust(cd,method="ward.D")

# Plot trees 
plot(groups_TF_IDF, hang=-1)

# Cut into 2 subtrees.
rect.hclust(groups_TF_IDF,6)

colors = c("red", "blue", "green", "black", "organge", "gray")
clus4 = cutree(groups_TF_IDF, 4)
plot(as.phylo(groups_TF_IDF), type = "fan", tip.color = colors[clus4],
     label.offset = 0.1, cex = 0.6)


############################### K-Means Clustering (frequency and cosine distance) ############################### 

# Rerun distance
d <- dist(m)

# Kmeans clustering 
kfit <- kmeans(d, 4, nstart=100)

# Plot cluster
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# Print contents of kfit
print(kfit)

# Print cluster sizes
kfit$size

# Print clusters members
kfit$cluster

# Sum of squared distance between cluster centers 
kfit$betweenss

# Sum of squared distance within a cluster 
kfit$withinss

# Determine optimal number of clusters
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# Use cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs
kfit <- kmeans(cd, 4, nstart=100)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# Print contents of kfit
print(kfit)

# Print cluster sizes
kfit$size

# Print clusters members
kfit$cluster

# Sum of squared distance between cluster centers 
kfit$betweenss

# Sum of squared distance within a cluster 
kfit$withinss

# Determine optimal number of clusters for cosine distance
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

############################### K-Means Clustering (TF-IDF and cosine distance) ############################### 

# Rerun distance
d <- dist(m_tfidf)

# Kmeans clustering 
kfit <- kmeans(d, 5, nstart=100)

# Plot cluster
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# Print contents of kfit
print(kfit)

# Print cluster sizes
kfit$size

# Print clusters members
kfit$cluster

# Sum of squared distance between cluster centers 
kfit$betweenss

# Sum of squared distance within a cluster 
kfit$withinss

# Determine optimal number of clusters
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# Use cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)
cd <- 1-cs
kfit <- kmeans(cd, 5, nstart=100)
clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# Print contents of kfit
print(kfit)

# Print cluster sizes
kfit$size

# Print clusters members
kfit$cluster

# Sum of squared distance between cluster centers 
kfit$betweenss

# Sum of squared distance within a cluster 
kfit$withinss

# Determine optimal number of clusters for cosine distance
wss <- 2:(length(docs)-1)
for (i in 2:(length(docs)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(2:(length(docs)-1), wss[2:(length(docs)-1)], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

############################### Topic Modeling using LDA ############################### 

# Start from a representative point
burnin <- 1000

# Perform 2000 iterations 
iter <- 2000

# Use Thinning to ensure that samples are not correlated.
thin <- 500

# 5 different, randomly chosen starting points
nstart <- 5

# Using seeds. 
seed <- list(1,2,3,4,5)

# Take the run with the highest probability as the result
best <- TRUE

#Number of topics 
k <- 6

# LDA
ldaOut <- LDA(dtm,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))

terms(ldaOut,15)
ldaOut.terms <- as.matrix(terms(ldaOut,8))

# Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 

#--------------------------- Plot LDA Top Terms for each Topic --------------------------- 
#1 Management of projects, systems, employees
#2 Statistical time estimation for projects using monte carlo
#3 Issue Based Information System (IBIS) , issu mapping
#4 Text mining
#5 Risk management, project management
#6 Statistical modeling

theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  # Center the title
  theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks = aticks, # Set axis ticks to on or off
        panel.grid.minor = pgminor, # Turn on or off the minor grid lines
        legend.title = lt, # Turn on or off the legend title
        legend.position = lp) # Turn on or off the legend
}


word_chart <- function(data, input, title) {
  data %>%
    # Set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    # Get the words, not the points
    geom_point(color = "transparent") +
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme_lyrics() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          # Axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    coord_flip()
}
# Number of words to visualize
num_words <- 15 

# Create function that accepts the lda model and num word to display
top_terms_per_topic <- function(lda_model, num_words) {
  
  # Tidy LDA object to get word, topic, and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  
  top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    # Get the top num_words per topic
    slice(seq_len(num_words)) %>%
    arrange(topic, beta) %>%
    # Row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    # Add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))
  # Create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
  # Call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$term, title)
}
# Call the function you just built!
top_terms_per_topic(ldaOut, num_words)

#--------------------------- Plot LDA Top Docs for each Topic--------------------------- 
# Number of top docs to view
number_of_documents = 7 
title <- paste("LDA Top Documents for", k, "Topics")

# Create tidy form showing topic, document and its gamma value
topics_tidy <- tidy(ldaOut, matrix = "gamma")

# Came process as used with the top words
top_documents <- topics_tidy %>%
  group_by(topic) %>%
  arrange(topic, desc(gamma)) %>%
  slice(seq_len(number_of_documents)) %>%
  arrange(topic, gamma) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  # Re-label topics
  mutate(topic = paste("Topic", topic, sep = " "))

title <- paste("LDA Top Documents for", k, "Topics")
word_chart(top_documents, top_documents$document, title)



