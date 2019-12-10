# NLP Workshop Session 3 - Topic Modelling Hands-On
# Load the tm library
library(tm)

#Simple text analysis example

text <- "I like to eat broccoli and bananas.
I ate a banana and spinach smoothie for breakfast.
Chinchillas and kittens are cute.
My sister adopted a kitten yesterday.
Look at this cute hamster munching on a piece of broccoli."

# create a corpus
a_corpus <- SimpleCorpus(VectorSource(text))

#remove punctuation
a_corpus <- tm_map(a_corpus, removePunctuation)
#Strip digits
a_corpus <- tm_map(a_corpus, removeNumbers)
#remove stopwords
a_corpus <- tm_map(a_corpus, removeWords, stopwords("english"))
#remove whitespace
a_corpus<- tm_map(a_corpus, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(a_corpus[[1]]))

#create the document-term matrix
a_dtm <- DocumentTermMatrix(a_corpus)
inspect(a_dtm)

# As per LDA, every Document is a mixture of Topics
# And every Topic is a mixture of Words

# For example, we could imagine a two-topic model of American news, 
# with one topic for “politics” and one for “entertainment.” 
# The most common words in the politics topic might be “President”, 
# “Congress”, and “government”, while the entertainment topic 
# may be made up of words such as “movies”, “television”, and “actor”. 
# Importantly, words can be shared between topics; a word like “budget” 
# might appear in both equally.

library(topicmodels)
# Run the LDA algorithm with k as 2 for generating a 2-topic model
lda <- LDA(a_dtm, k = 2)
# view the topics
lda.topics <- as.matrix((topics(lda,6)))
topics(lda)

#view the terms
lda.terms <-as.matrix(terms(lda))
terms(lda)

# Topic modelling with news articles date set 
library(tm)
library(dplyr)
data("acq")
acq

# first document
acq[[1]]

# data pre-processing

#remove punctuation
acq <- tm_map(acq, removePunctuation)
#Strip digits
acq <- tm_map(acq, removeNumbers)
#remove stopwords
acq <- tm_map(acq, removeWords, stopwords("english"))
acq <- tm_map(acq, removeWords, "said")
#remove whitespace
acq <- tm_map(acq, stripWhitespace)
#Good practice to check every now and then
writeLines(as.character(acq[[30]]))

library(tidytext)
# convert ot tody format
acq_td <- tidy(acq)
acq_td

# separate into words
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

# create a DTM
acq_dtm <- DocumentTermMatrix(acq)

inspect(acq_dtm)

# Run LDA with k = 5
# set a seed so that the output of the model is predictable
acq_lda <- LDA(acq_dtm, k = 2, control = list(seed = 1234))
acq_lda

#list the topics
acq_lda.topics <- as.matrix((topics(acq_lda,6)))
topics(acq_lda)

#list the terms
acq_lda.terms <-as.matrix(terms(acq_lda))
terms(acq_lda)

# The tidytext package provides the method tidy() for extracting the per-topic-per-word probabilities, called  
# β (“beta”), from the model.

library(tidytext)

acq_topics <- tidy(acq_lda, matrix = "beta")
acq_topics

# This has turned the model into a one-topic-per-term-per-row format. 
# For each combination, the model computes the probability of that term 
# being generated from that topic. 
# plot the top terms in the dataset
library(ggplot2)
library(dplyr)

acq_top_terms <- acq_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

acq_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# we can filter for relatively common words, such as those that have a  
# β greater than 1/1000 in at least one topic
# calculate the beta values 
#beta_spread <- acq_topics %>%
#  mutate(topic = paste0("topic", topic)) %>%
#  spread(topic, beta) %>%
#  filter(topic1 > .001 | topic2 > .001) %>%
#  mutate(log_ratio = log2(topic2 / topic1))

#  ggplot(beta_spread,aes(log_ratio,term)) +
#  geom_col(show.legend = FALSE) 
  
# LDA also models each document as a mixture of topics. We can examine the per-document-per-topic probabilities, called  
# “gamma”, with the matrix = "gamma" argument to tidy()  
# Computing the gamma values  
acq_documents <- tidy(acq_lda, matrix = "gamma")
  acq_documents

# Topic modelling example wih Jane Austen books  
  library(janeaustenr)
  library(dplyr)
  library(tidytext)
  # load the data from six of Jane Austen's books
  austen <- austen_books()
  
  # separate text into word tokens
  words <- austen %>%
    unnest_tokens(word, text)
  
  # Remove the stop words and list the words according to their counts
  word_counts <- words %>%
    anti_join(stop_words) %>%
    count(book, word, sort = TRUE) %>%
    ungroup()
  
  # List the top 10 words
  top_n(word_counts,10)
  
  # All the top words are nouns, we will add them as stop words and remove from analysis
  # stopwords <- add_row(stop_words, word = c("fanny","emma", "elinor","miss","elizabeth","crawford","marianne","anne","catherine"), lexicon = c("SMART", "SMART", "SMART", "SMART", "SMART", "SMART", "SMART", "SMART", "SMART"))
  
  # find document-word counts after removal of common nouns
  word_counts <- words %>%
    anti_join(stopwords) %>%
    count(book, word, sort = TRUE) %>%
    ungroup()
  
  top_n(word_counts,10)
  
  #create a document-term matrix with the text
  austen_dtm <- word_counts %>%
    cast_dtm(book, word, n)
  
  austen_dtm
  
  library(topicmodels)
  # run the LDA algorithm on the Austen DTM
  austen_lda <- LDA(austen_dtm, k = 5, control = list(seed = 1234))
  
  
  austen_topics <- tidy(austen_lda, matrix = "beta")
  
  # list the most common topics
  top_n(austen_topics, 10)
  
  #list the top terms
  top_terms <- austen_topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  library(ggplot2)
  # plot the top terms 
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  # Compute the gamma values
  austen_gamma <- tidy(austen_lda, matrix = "gamma")
  
  top_n(austen_gamma, 10)
  
  
  austen_classifications <- austen_gamma %>%
    group_by(topic, document) %>%
    top_n(1, gamma) %>%
    ungroup()
  
  book_topics <- austen_classifications %>%
    count(document, topic) %>%
    group_by(topic) %>%
    top_n(1, n) %>%
    ungroup() %>%
    transmute(consensus = book, topic)
  
  austen_classifications %>%
    inner_join(book_topics, by = "topic") %>%
    filter(term != consensus) 
  