# NLP_AI_Workshop
The repo consists of files used in Natural Language Processing Intro workshop using R at URI during Fall 2019

Session 1 - Intro to NLP

Performed basic string manipulation, tokenization, lemmatization on text data

In Session 1, we start with basic data types in R. We focus mainly on String data types and functions to manipulate them, which are essential for Text Analysis. We then proceed to the in-class example and perform some tokenization and lemmatization of text.

Session 2 - Intro to Text Mining

Introduced some basic text mining functions, word clouds and Sentiment Analysis

In Session 2 hands-on, we start with basic functions like filter, arrange, inner_join, anti_join in the “dplyr” package which is essential for data wrangling in R. We will proceed to look at how to generate some simple plots. Then we will perform some text mining with a dataset of literature works. This includes data pre-processing and removal of stop words. We analyze the word and document frequencies (tf-idf) and check which which words are more important. In Sentiment Analysis, we explore some methods and dictionaries that exist for evaluating the opinion or emotion in text. Eg., AFINN, bing, nrc. We plot wordclouds of most common words as well as most common postive and negative words appearing in the text.

Session 3 - Topic Modelling

Performed topic modelling using the LDA algorithm and analyzed results

In this session hands-on, we first load a text corpus and perform some text pre-processing and cleaning.
Then we convert the corpus into tidy text and a Document Term Matrix.
We run the LDA algorithm with number of topics k = 2 on the DTM. We calculate Beta values, the per-topic-per-word probabilities and 
also the Gamma values, per-document-per-topic probabilities, and analyze the results.

