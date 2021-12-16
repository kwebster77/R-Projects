##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### PDF Content
##### Version 0.1

#install.packages("pdftools")
library(pdftools)
library(magrittr)
library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

#01. Importing pdf content
files <- list.files(pattern = "pdf$")
content <- lapply(files, pdf_text)
my_content <- as.data.frame(content)
length(my_content)

lapply(my_content, length) 
my_content


#02. Putting the vector in a data frame
mydf <- data.frame(line=1:6, text=my_content$c..The.Global.Business.nSchool.nHult.....is....more.....than...a.n.nbusiness......school....It.s...a.n.nglobal........network......that.n...)
print(mydf) #5 observation and 2 variables

#03. Tokenizing the mydf dataframe
token_list <- mydf %>%
  unnest_tokens(word, text) #takes the text and creates a vector at word level stript_punct = T
print(token_list)

#04. Token frequencies
frequency_tokens <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  count(word, sort =TRUE) #Counts how many time the word appears
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
  count(word, sort =TRUE) %>% #Eliminates the stop words from the data frame
  mutate(word=reorder(word, n)) %>% #Updates the word variable in a descending order
  #Plotting the counting words graph
  ggplot(aes(word, n, fill=word))+ #words in x and n count in y, the fill will give the color if wanted by the choosen variable
  geom_col()+ #creates bar charts, if we want histograms we should use geom_histogram(binwidth=XX)
  labs(x = "words in text", y = "number of repetition")+ #Creating the title axes for x and y
  ggtitle("Words count by word distribution") + #Creating the title of the graph
  theme_bw()+ #choosing the theme for the graph
  coord_flip() #flipping from vertical to horizontal
print(freq_hist)


#PERSONAL: Tokenizing the pdf document from the class exercise 

#install.packages("pdftools")
library(pdftools)
library(magrittr)
library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

#01. Importing pdf content. Before running this step Go to the menu Session --> Set Working Directory --> Choose directory (choose the path where you have downloaded the document).

files <- list.files(pattern = "pdf$")
content <- lapply(files, pdf_text)
my_content <- as.data.frame(content)
length(my_content)

lapply(my_content, length) 
my_content

#02. Putting the vector in a data frame
mydf <- data.frame(line=1:6, text=my_content$X.To.view.the.full.contents.of.this.document..you.need.a.later.version.of.the.PDF.viewer..You.can.upgrade.nto.the.latest.version.of.Adobe.Reader.from.www.adobe.com.products.acrobat.readstep2.html.n.nFor.further.support..go.to.www.adobe.com.support.products.acrreader.html.n.)
print(mydf) #5 observation and 2 variables

#03. Tokenizing the mydf dataframe
token_list <- mydf %>%
  unnest_tokens(word, text) #takes the text and creates a vector at word level stript_punct = T
print(token_list)

#04. Token frequencies
frequency_tokens <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  count(word, sort =TRUE) #Counts how many time the word appears
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
  count(word, sort =TRUE) %>% #Eliminates the stop words from the data frame
  mutate(word=reorder(word, n)) %>% #Updates the word variable in a descending order
  #Plotting the counting words graph
  ggplot(aes(word, n, fill=word))+ #words in x and n count in y, the fill will give the color if wanted by the choosen variable
  geom_col()+ #creates bar charts, if we want histograms we should use geom_histogram(binwidth=XX)
  labs(x = "words in text", y = "number of repetition")+ #Creating the title axes for x and y
  ggtitle("Words count by word distribution") + #Creating the title of the graph
  theme_bw()+ #choosing the theme for the graph
  coord_flip() #flipping from vertical to horizontal
print(freq_hist)

#01, Importing all PDF files from the same folder
#install.packages("pdftools")
library(pdftools) # we need this library to use pdf_text
setwd("/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/pdf")
nm <- list.files(path="/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/pdf")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))


#02. Importing pdf content. Before running this step Go to the menu Session --> Set Working Directory --> Choose directory (choose the path where you have downloaded the document).

#files <- list.files(pattern = "pdf$")
#content <- lapply(files, pdf_text)
my_content <- as.data.frame(my_pdf_text)
length(my_content)
lapply(my_content, length) 
my_content

#03. Putting the vector in a data frame
mydf <- data.frame(line=1:18, text=my_content$V1)
print(mydf) #5 observation and 2 variables


#06. Token frequency bar chart
freq_hist <- mydf %>%
  unnest_tokens(word,text) %>% #Creates the single words
  anti_join(stop_words) %>% #Eliminates the stop words from the data frame
  count(word, sort =TRUE) %>% #Eliminates the stop words from the data frame
  mutate(word=reorder(word, n)) %>% #Updates the word variable in a descending order
  #Plotting the counting words graph
  ggplot(aes(word, n, fill=word))+ #words in x and n count in y, the fill will give the color if wanted by the choosen variable
  geom_col()+ #creates bar charts, if we want histograms we should use geom_histogram(binwidth=XX)
  labs(x = "words in text", y = "number of repetition")+ #Creating the title axes for x and y
  ggtitle("Words count by word distribution") + #Creating the title of the graph
  theme_bw()+ #choosing the theme for the graph
  coord_flip() #flipping from vertical to horizontal
print(freq_hist)







