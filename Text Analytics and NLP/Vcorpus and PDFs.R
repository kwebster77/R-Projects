##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### Vcorpus and PDFs
##### Version 0.1




library(tm)
data("acq")#50 articles from Reuters
acq # we get a VCorpus

#We want to convert this to a tidy format that has 
#one row per document

acq_tidy <- tidy(acq)
summary(acq_tidy)

acq_tokens <- acq_tidy %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = 'word')
#most common words
acq_tokens %>%
  count(word, sort=TRUE)

######### Corpus object with PDF files

#Import the PDF files that you downloaded from mycourses
library(pdftools) # we need this library to use pdf_text
library(tm)
setwd("/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/PDF Files_Exam")
nm <- list.files(path="/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/PDF Files_Exam")

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

opinions # I want to see the VCoprus content
opinions[[4]] # I want the first doc, this is a list so [[]] needs to be applied
#let's take a look at the Corpus that was created 
#if you want to get some metadata from the i-th object:
opinions[[7]]$meta$author
opinions[[4]]$meta$id
#Exercises:
#Try to find the author's name for the 7th document
#try to find the document ID in for the 4th document
