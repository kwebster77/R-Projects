##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### Tokenization and frequencies
##### Version 0.1

#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("stringr")

library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

#01. Creating a vector with long sentences
my_txt <- c("I think that data is the new bacon", 
            "What is more. I think text analytics is the new turkey bacon", 
            "very few people know how good Canadian bacon is",
            "even fewer people know how a beaver tails tastes like",
            "Putin french fries are not so good")

#02. Putting the vector in a data frame
mydf <- data.frame(line=1:5, text=my_txt)
print(mydf) #5 observation and 2 variables


#In case we want to upload a file: To be confirm 
#Mydf <- data.frame(xyz.csv)
#Mydf<-data.frame(path.csv)

#03. Tokenizing the mydf dataframe
token_list <- mydf %>%
  unnest_tokens(word,text) #takes the text and creates a vector at word level (only one word), takes out the punctuation. To maintain punctuation include it, unnest_tokens(word, text, token = "words / characters / sentences", stript_punct = F)
print(token_list)

#04. Token frequencies
frequency_tokens <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  count(word, sort =TRUE) %>%  #Counts how many time the word appears
  drop_na() #helps to drop na if existed

print(frequency_tokens) #n stands for frequency

#05. Removing stop words (I, you, me, the, of, etc)
data(stop_words)

frequencies_tokens_nostop <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  anti_join(stop_words) %>% #Eliminates the stop words from the data frame
  count(word, sort =TRUE) #Counts how many time the word appears without the stop words

print(frequencies_tokens_nostop)

#06. Token frequency bar chart
freq_hist <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  anti_join(stop_words) %>% #Eliminates the stop words from the data frame
  count(word, sort =TRUE) %>% #Counting words frequency base on "n" number of times the word appears
  mutate(word=reorder(word, n)) %>% #Updates the word variable in a descending order
  #Plotting the counting words graph
  ggplot(aes(word, n, fill=word))+ #words in x and n count in y, the fill will give the color if wanted by the choosen variable
  geom_col()+ #creates bar charts, if we want histograms we should use geom_histogram(binwidth=XX)
  labs(x = "words in text of the reading", y = "number of repetition")+ #Creating the title axes for x and y
  ggtitle("Words count by word frequency") + #Creating the title of the graph
  theme_bw()+ #choosing the theme for the graph
  coord_flip() #flipping from vertical to horizontal

print(freq_hist)







