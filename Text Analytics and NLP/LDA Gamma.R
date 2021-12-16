##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### LDA Gamma
##### Version 0.1



##### Text Analytics: CLASS 06 - Machine Learning - LDA Gamma-----
### After class practice: Latent Dirichlet algorithm

library("tidytext")
library("tidyverse")
library("tidyr")
library("tidytuesdayR")
library("stringr")
library("textreadr")
library("pdftools")
library("textshape")
library("twitteR")
library("tm")
library("ggplot2")
library("scales")
library("magrittr")
library("dplyr")
library("gutenbergr")
library("Matrix")
library("textdata")
library("igraph")
library("ggraph")
library("widyr")
library("topicmodels")
library("quanteda")
library("quanteda.textmodels")
library("RColorBrewer")


###### LDA per chapter based on Gutenber

#Retreiving data from the Gutenberg project
library(gutenbergr)
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#devide into documents, each representing one chapter
library(stringr)
reg <-regex("^chapter ", ignore_case=TRUE)
by_chapter <- books %>% #dividing the books by chapter
  group_by(title) %>% #grouping by title
  mutate(chapter=cumsum(str_detect(text,reg))) %>% #creating the chapter as a new column
  ungroup() %>% #ungrouping for a text analysis
  filter(chapter>0) %>%
  unite(document, title, chapter)

#split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text) #tokenizing

#find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>% #removing stop words
  count(document, word, sort = TRUE) %>% #counting frequency
  ungroup() #ungrouping for the LDA analysis

print(word_counts)

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n) #creating the dtm format 

chapters_lda <- LDA(chapters_dtm,k=4, control = list(seed=1234)) #creating the LDA ranking, K means the number of topics we want to predict
chapters_lda

#### Per documnet classification

#Putting the chapters back together in the correct book. 
#Beta means per-topic-per-word probability
#Gamma means per-document-per-topic probability
chapters_gamma <- tidy(chapters_lda, matrix="gamma")
chapters_gamma 

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"),sep = "_", convert=TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title=reorder(title, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()+
  facet_wrap(~title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications


