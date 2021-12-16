##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### N-Grams and tokenization
##### Version 0.1


#install.packages("janeaustenr")
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)



#unnest_tokens(word, text)
#unnest_tokens(bigram, text, token = "ngrams", n=2)
#unnest_tokens(quadrogram, text, token = "ngrams", n=4)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>% 
  drop_na()
#tokenizing by pairs = bigrams (input: bigram, text, token="": ngrams, n=#:2)
#the n is what mathers the most

austen_bigrams 
#We want to see the bigrams (words that appear together, "pairs")

austen_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
#What bigrams are the most frequent? stop words

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
#we are separating the bigrams into "word1" and "word2" separated by a space " "
#This would give us two separated tokens 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #%in% this would help us to filter out word1 in the stop word and removing them 
  filter(!word2 %in% stop_words$word) #%in% this would help us to filter out word2 in the stop word and removing them

#creating the new bigram, "no-stop-words":
bigram_counts  <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts


####### VISUALISING A BIGRAM NETWORK

#install.packages("igraph") --> graphs and network analysis
#packageDescription("igraph")

library(igraph) 
bigram_graph <- bigram_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

#Creating the arrow open will be "➔" while close will be like "➡︎" 
a <- grid::arrow(type="open", length = unit(.10,"inches")) 

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") + #"fr" means frequency
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a)+ #including the arrow direction to the graph
  geom_node_point(color="#009999", size = 2)+ #changing color and size of the node http://www.sthda.com/english/wiki/colors-in-r  
  geom_node_text(aes(label=name), vjust =1, hjust=1) + #label or word in this case
  theme_gray() #choosing the theme for the graph


###### What if we are interested in the most common
################ 4 consecutive words - quadro-gram

quadrogram <- austen_books() %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>% #tokenizing
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>% #separating quadrogramas by ""
  drop_na() %>% #dropping nas
  filter(!word1 %in% stop_words$word) %>% #filtering stop words in word1
  filter(!word2 %in% stop_words$word) %>% #filtering stop words in word2
  filter(!word3 %in% stop_words$word) %>% #filtering stop words in word3
  filter(!word4 %in% stop_words$word)     #filtering stop words in word4

quadrogram

#Counting and filtering quadrograms

quadrogram_graph  <- quadrogram %>%
  count(word1, word2, word3, word4,  sort = TRUE) %>% #counting quadrogrmas 
  filter(n>1) %>% #filtering 
  graph_from_data_frame()

quadrogram_graph

#Creating the arrow open will be "➔" while closed will be like "➡︎" 
b <- grid::arrow(type="closed", length = unit(.08,"inches")) 

ggraph(quadrogram_graph, layout = "fr") + #"fr" means frequency
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = b)+ #including the arrow direction to the graph
  geom_node_point(color="#FF99FF", size = 2)+ #changing color and size of the node http://www.sthda.com/english/wiki/colors-in-r  
  geom_node_text(aes(label=name), vjust =3, hjust=3) + 
  theme_light() #choosing the theme for the graph



