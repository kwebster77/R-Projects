
##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### DTM format
##### Version 0.1


#update.packages()
#install.packages("slam", type = "binary")
#install.packages(c("NLP", "tm", "scales"))
#packageDescription("xxxx") #This code allows to read the description of what the package is about

#Calling libraries 

library(pdftools) #Utilities based on 'libpoppler' for extracting text, fonts, attachments and metadata from a PDF file
library(magrittr) #Provides a mechanism for chaining commands with a new forward-pipe operator, %>%
library(stringr) #All function and argument names (and positions) are consistent, all functions deal with "NA"'s and zero length vectors in the same way, and the output from one function is easy to feed into the input of another.
library(tidytext) #make many text mining tasks easier, more effective, and consistent with tools already in wide use
library(tidyverse) #The 'tidyverse' is a set of packages that work in harmony because they share common data representations and 'API' design.
library(dplyr) #A fast, consistent tool for working with data frame like objects, both in memory and out of memory
library(ggplot2) #You provide the data, tell 'ggplot2' how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details
library(ggthemes) #Some extra themes, geoms, and scales for 'ggplot2'
library(tidytuesdayR) #Project where weekly dataset is post in a public data repository
library(NLP) #Basic classes and methods for Natural Language Processing.
library(tm)#A framework for text mining applications within R.
library(scales) #Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends. Benchmark

#### DTM object using Associated Press articles 

tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles

#let's look at the data
View(netflix[1:40,])
unique(netflix$title)
unique(netflix$country)

#install.packages("topicmodels")
data("AssociatedPress", package = "topicmodels")
AssociatedPress

#99% of the document-word pairs are zero
terms <- Terms(AssociatedPress)
terms
ap_td <- tidy(AssociatedPress)
ap_td

#####Converting back from Tidy to DTM
ap_td %>%
  cast_dtm(document, term, count )

#####Putting the data in a sparse matrix
library(Matrix)
n <- ap_td %>%
  cast_sparse(document, term, count)
class(n)
dim(n)

#####Converting Jane Austen to DTM
##### Netflix DTM

library(tidytuesdayR)
library(dplyr)
library(tidytext)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_dtm <- netflix %>%
  unnest_tokens(word, text) %>%
  count(title, word) %>%
  cast_dtm(title, word, n) # 

netflix_dtm

# sparsity: 100% Show the sparsity (as a count or proportion) of a matrix. 
#For example, . 99 sparsity means 99% of the values are zero. 
#Similarly, a sparsity of 0 means the matrix is fully dense.


#### In class
#01 Downloading Netflix data from the tidytuesdayR package

tuesdata <- tidytuesdayR::tt_load('2021-04-20') #by using the tt_load we will download the data on the date selected
netflix <- tuesdata$netflix_titles

### Look at the data with the descriptive title and countries
View(netflix[1:40,]) #We are calling the first 40 rows for all the variables (columns)
unique(netflix$title)
unique(netflix$country)

#02 Renaming the description variables as "text"
colnames(netflix)[12] <- "text" #this is good coding practice ( the variable with the unstructured data has to be name text)

#03 Tokenizing: unnest_token()
tidy_netflix <- netflix %>%
  unnest_tokens(word, text)
print(tidy_netflix)

#04 removing stop words: anti_join()
data(stop_words)
netflix_no_stop <- tidy_netflix %>%
  anti_join(stop_words)
print(netflix_no_stop)

#05 printing the count frequencies for each token without stop words
netflix_no_stop %>%
  count(word, sort=TRUE)

#06 plotting the token frequencies
freq_hist <-netflix_no_stop %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n, fill=word))+ #by using the fill variable we can change the colors of the bars 
  geom_col()+ #creates bar charts
  labs(x = "words in text", y = "number of repetition")+ #Creating the title axes for x and y
  ggtitle("Words count by word distribution") + #Creating the title of the graph
  theme_bw()+ #choosing the theme for the graph
  coord_flip() #flipping from vertical to horizontal
print(freq_hist)

#07 creating a tidy format for selected countries (USA, BR, IN)

### United States movies
usa <- netflix %>%
  filter(country== "United States")

tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

### Brazil movies
brazil <- netflix %>%
  filter(country== "Brazil")

tidy_brazil <- brazil %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_brazil)

### India movies
india <- netflix %>%
  filter(country== "India")

tidy_india <- india %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_india)

#08 Combining all the datasets and creating frequencies
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_brazil, author= "Brazil"),
                       mutate(tidy_india, author="India")
)%>%#closing bind_rows. The output of bind_rows() will contain a column if that column appears in any of the inputs. bind_rows(list(one, two), .id = "id name if wanted")
  mutate(word=str_extract(word, "[a-z']+")) %>% #extracting matching patterns ???
  count(author, word) %>% #lets you quickly count the unique values of one or more variables
  group_by(author) %>% #group_by() takes an existing tbl and converts it into a grouped tbl where operations are performed "by group". ungroup() removes grouping.
  mutate(proportion = n/sum(n)) %>% #Calculating the sparsity:  k ≪ N of the coefficients cn are nonzero, which enables the transform to compress the spike energy into very few coefficients.
  select(-n) %>% #Selecting everything except the sparsity column
  spread(author, proportion) %>% #Consolidates data, spread(data: dataframe, key: values that will become the variable names, value: column for the new variables)
  gather(author, proportion, `Brazil`, `India`) #Takes data by the key variables we need. gather(data, key, value, ...: is a way to specify what columns to gather from.)
#http://statseducation.com/Introduction-to-R/modules/tidy%20data/spread/ 




