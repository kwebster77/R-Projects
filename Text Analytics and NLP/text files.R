##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### text files
##### Version 0.1


### Importing data from a directory

#install.packages("pdftools")
library(pdftools)
library(magrittr)
library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

#Text format files
#install.packages("textreadr")
library(textreadr)
#Importing all .txt files from one directory # a txt works like a csv file with multiple rows
setwd("/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/Txt files")

nm <- list.files(path="/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/Txt files")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

#Importing all .doc files from one directory
#install.packages("textshape") #for some reason textreadr has issues getting textshape
#install.packages("textreadr")

library(textreadr)
setwd("/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/Txt files")
nm <- list.files(path="/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/Txt files")
my_doc_text <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))

