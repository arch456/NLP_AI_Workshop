# NLP Workshop - Session 2 Hands-On

# Dplyr basics

# 1. Count() function - counts the number of observations of a specific value
a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
df <- data.frame(first=a,
                 second=b,
                 rank=levels)
df

df %>% 
  count(first)

df %>% 
  count(rank, sort = TRUE)

# arrange() function is used to sort columns. By default, it sorts in ascending order
# for descending order, we need to specify 'desc'

df %>%
  arrange(second)

df %>%
  arrange(desc(first))

# filter() function is used to filter rows in the data

df %>%
  filter(rank == "A")
# This gives only the rows with rank "A"

df %>%
  filter(first == "2")
# This gives only the rows with first column as "2"

# join() function is used to join two data sets

df1 = data.frame(ID = c(1, 2, 3, 4, 5),
                 w = c('a', 'b', 'c', 'd', 'e'),
                 x = c(1, 1, 0, 0, 1),
                 y=rnorm(5),
                 z=letters[1:5])

df2 = data.frame(ID = c(1, 7, 3, 6, 8),
                 a = c('z', 'b', 'k', 'd', 'l'),
                 b = c(1, 2, 3, 0, 4),
                 c =rnorm(5),
                 d =letters[2:6])

df3 = inner_join(df1, df2, by = "ID")

# Inner_join() combines both the data frames and 
# generates a new data frame which has only the rows belonging to the common values of "ID" column

# Basic Plotting in R

library(ggplot2)

# plot with points
df %>%
  ggplot(aes(x = first,y = second)) +
  geom_point()

# bar plot with points
df %>%
  ggplot(aes(x = first,y = second)) +
  geom_col()

#specifying color
df %>%
  ggplot(aes(x = first,y = second)) +
  geom_col(fill = "blue") 

# swapping the coordinates
df %>%
  ggplot(aes(x = first,y = second)) +
  geom_col(fill = "blue") +
  coord_flip()

# You can modify the labels, title etc.

#################################################################################################3

# Let's perform Text Analysis on our example, the works of Jane Austen

#Text Analysis process

#1. Collecting Raw text

#install.packages("janeaustenr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("gutenbergr")
# install.packages("tm")

library(janeaustenr)
library(dplyr)
library(stringr)


original_books <- austen_books()
# text data can be represented as tidy text

# Another way to load the books 
library(gutenbergr)
austen <- gutenberg_download(c(1342,161,158,121,141))

library(tm)
# Creating a corpus vector
a_corpus <- SimpleCorpus(VectorSource(austen$text))

# text data can be converted into a corpus
# cleaning the corpus
a_corpus <- tm_map(a_corpus, content_transformer(removePunctuation))

a_corpus <- tm_map(a_corpus, content_transformer(removeNumbers))

a_corpus  <- tm_map(a_corpus, content_transformer(stemDocument), language = "english")

# 2.Representing Text

# To work with this as a tidy dataset, we need to restructure it in the one-token-per-row format

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

#This function uses the tokenizers package to separate each line of text in the original data frame into tokens. 

tidy_books

# remove stop words with an anti_join function.
tidy_books <- tidy_books %>%
  anti_join(stop_words)


# 3. Analyzing word and document frequency: tf-idf

# One measure of how important a word may be is its term frequency (tf) -  
# how frequently a word occurs in a document

# Another approach is to look at a term’s inverse document frequency (idf), 
# which decreases the weight for commonly used words and increases the weight for words 
# that are not used very much in a collection of documents. 
# This can be combined with term frequency to calculate a term’s tf-idf 
# (the two quantities multiplied together), the frequency of a term adjusted for how rarely it is used.

# The statistic tf-idf is intended to measure how important a word is to a document in a collection (or corpus) of documents, for example, 
# to one novel in a collection of novels or to one website in a collection of websites.

# Term frequency in our data (tf)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# we can use the already tidied data variable tidy_books

word_count <- tidy_books %>%
  count(word, sort = TRUE)

# counting with book name
book_words <- tidy_books %>%
  count(book, word, sort = TRUE) 

book_words
# There is one row in this book_words data frame for each word-book combination
# n is the number of times that word is used in that book and total is the total words in that book. 
# The usual words are here with the highest n, “the”, “and”, “to”, and so on

# The bind_tf_idf function:

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

# Notice that idf and thus tf-idf are zero for these extremely common words. 
# These are all words that appear in all six of Jane Austen’s novels, 
# so the idf term (which will then be the natural log of 1) is zero. 
# The inverse document frequency (and thus tf-idf) is very low (near zero) 
# for words that occur in many of the documents in a collection; 
# this is how this approach decreases the weight for common words.

book_words %>%
  arrange(desc(tf_idf))

library(ggplot2)
# The plot shows a visualization of the most common words in these books
word_count %>% 
  filter(n > 600) %>%
    ggplot(aes(word,n)) +
      geom_col(fill = "blue")+
        coord_flip()

# Here we see all proper nouns, names that are in fact important in these novels. 

# 4. Sentiment Analysis
#  There are a variety of methods and dictionaries that exist for evaluating the opinion or emotion in text. 
# Eg., AFINN, bing, nrc

# install.packages("textdata")
library(textdata)
library(tidytext)
library(dplyr)
library(ggplot2)
get_sentiments("nrc")

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

emma_words <- tidy_books %>%
  filter(book == "Emma")

emma_words_joy <- emma_words %>% 
  inner_join(nrc_joy)

emma_words_joy_count <- emma_words_joy %>%
  count(word, sort = TRUE)

# Or this can be specified in the below manner:
# emma_words_joy_count <- tidy_books %>%
#   filter(book == "Emma") %>%
#     inner_join(nrc_joy) %>% 
#       count(word, sort = TRUE)


bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts

# Most common positive and negative words

poswords <- filter(bing_word_counts,sentiment == "positive" & n >150)

poswords %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "chartreuse3") +
  coord_flip()

negwords <- filter(bing_word_counts,sentiment == "negative" & n >100)    

negwords %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "brown3") +
  coord_flip()

# 5. Gain Insights
# Word clouds with the most frequent words
# install.packages("wordcloud")
library(wordcloud)
library(reshape2)

word_count %>%
  with(wordcloud(word, n, max.words = 100))

# Specifying colors
word_count %>%
  with(wordcloud(word, n, colors = c("red","blue","green"), max.words = 100))

# Plotting a Comparison Cloud

bing_word_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown4","chartreuse4"),
                   max.words = 100)


#library(tidyr)

#bing_word_counts %>%
#  count(book, index = linenumber %/% 80, sentiment) %>%
#  spread(sentiment, n, fill = 0) %>%
#  mutate(sentiment = positive - negative)

#library(ggplot2)

#ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
#  geom_col(show.legend = FALSE) +
#  facet_wrap(~book, ncol = 2, scales = "free_x")

# We can see how the plot of each novel changes toward more positive or negative sentiment over the trajectory of the story.

