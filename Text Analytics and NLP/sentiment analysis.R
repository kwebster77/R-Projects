##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### Sentiment Analysis
##### Version 0.1


#01 Creating the DTM format from previous section
library(tidytext)
library(tidyverse)
library(dplyr)
library(tidyr)

netflix %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(country, word) %>% #until here without the pipping sign, it is the tidy format
  cast_dtm(country, word, n) #makes all numeric, this is the dtm format
#<<DocumentTermMatrix (documents: 682 countries, terms: 18432 tokens)>>
#Sparsity: 100%


#Pre class videos


# Sentiment in tidytext()
#install.packages("textdata")
library(textdata)
library(tidytext)
afinn <- get_sentiments("afinn")#from -5 to +5 and 0 means neutral
nrc <- get_sentiments("nrc") #gives the flavors
bing <- get_sentiments("bing") #positive or negative

library(dplyr)
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"), #combining all the data source into one 
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)


sentiments %>%
  filter(lexicon == "bing") #binary, while nrc has the flavors

unique(sentiments$sentiment) #this is the qualitative sentiment that we can get
summary(sentiments$value) # this is 3rd score that we can use / quantitative


##### Lets take a look at the lexicons one by one

#how can we subset the data to get distinct lexicons?

bing_sentiment <- subset(sentiments, lexicon == "bing")
filter(bing_sentiment, sentiment == "negative") # these are the nrc options of sentiment labels

## This is a code starter for your take home exercise
## If you want to practice the code on other lexicons
#1 : Take a look at the sentiment for bing
bing_data <- subset(sentiments, lexicon == "bing")
unique(bing_data$sentiment)

#2 Take a look at the sentiment value for afinn
afinn_data <- subset(sentiments, lexicon == "afinn")
summary(afinn_data$value)

###IN CLASS 
### Sentiment analysis with Netflix

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_token <- netflix %>%
  unnest_tokens(word, text) #Tokenize data

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") #filter by surprise

#inner joining the India movies and the surprise sentiments
netflix_token %>% #This is a frequency table on tidyformat
  filter(country == "India") %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

##### Comparing different sentiment libraries on Netflix

india <- netflix_token %>%
  filter(country == "India")

afinn <- india %>%
  inner_join(get_sentiments("afinn"))%>% #Bringing the afinn library
  summarise(sentiment=sum(value)) %>% #summarise the data
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  india%>%
    inner_join(get_sentiments("bing"))%>% #combine the tokenize india with Bing
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>% #how they interact with eachother in a graph
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

######## Most common positive and negative words

bing_counts <- india %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() #to analyze data

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  ggtitle("Sentiment frequency on bing library") + #Creating the title of the graph
  theme_bw()+
  coord_flip()




