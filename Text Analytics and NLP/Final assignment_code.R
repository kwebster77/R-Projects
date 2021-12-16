##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN1 HULT 2021
##### Exam Questions 
##### Version 1.0

#libraries 

library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)


#####################################
##########Question 1

#01. Creating a vector with long sentences
my_txt_exam <- c("Fisher’s book is a wonderful corrective to the prejudice we 
have about the Barbary Coast pirates. It almost goes too far. It suggests that the West has
approached the idea of North African pirates with a prejudiced eye, because they are Islamic. 
Fisher says: why don’t we look at how the West related to North Africa and particularly the four 
Barbary states. These were Morocco and the three Ottoman states of North Africa – 
Algiers, Tunisia and what we would call Libya.", "I think that people still have that prejudice 
today. Just look at what’s going on in Somalia. It’s all part of a subconscious racism. 
These are black Muslims so we don’t romanticise them", "In the past slavery was central to 
Barbary Coast pirates. These pirates were much more considerate than the 
pirates of the Caribbean who were after loot. The Barbary pirates didn’t 
kill people because they wanted to sell them. The same tactics are being 
used today. The pirates want to intimidate people without doing them harm. 
They want ransom. They relied on shock and awe and the Somali pirates do 
exactly the same thing.", "There’s this great statistic that in the 17th century 
alone about a million Europeans were sold into slavery. But let’s not forget 
the same was true for North Africa – around the same number were also sold 
into slavery in southern Europe in places like Granada in Spain.")

#02. Putting the vector in a data frame
mydf_exam <- data.frame(line=1:4, text=my_txt_exam)
print(mydf_exam) #5 observation and 2 variables

#03. Tokenizing the the dataframe
token_list_exam <- mydf_exam %>%
  unnest_tokens(word,text) 
print(token_list_exam)

#04. Token frequencies
frequency_tokens_exam <- mydf_exam %>%
  unnest_tokens(word,text) %>% 
  count(word, sort =TRUE) %>% 
  drop_na()

print(frequency_tokens_exam) #printing frequency tokens (all)

#05. Removing stop words
data(stop_words)

frequencies_tokens_nostop_exam <- mydf_exam %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort =TRUE)

print(frequencies_tokens_nostop_exam)

#06. Token frequency bar chart
freq_hist_exam <- mydf_exam %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort =TRUE) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n, fill=word))+ 
  geom_col()+ 
  labs(x = "words in text of the reading", y = "number of repetition")+
  ggtitle("Word frequency for Exam Text") + 
  theme_bw()+ 
  coord_flip() 

print(freq_hist_exam)


#####################################
##########Question 2

library(pdftools) 
library(tm)

#01. Import the PDF files 
setwd("/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/PDF Files_Exam")
nm <- list.files(path="/Users/karleywebster/Documents/Education/Text Analytics and Natural Language Processing/PDF Files_Exam")

#02. Function to read all PDF 
Rpdf_exam <- readPDF(control = list(text = "-layout"))
opinions_exam <- Corpus(URISource(nm), 
                        readerControl = list(reader = Rpdf_exam))

#04. all content
opinions_exam

#05. find the metadata 
opinions_exam[[2]]$meta$id
opinions_exam[[7]]$meta$language


#####################################
##########Question 3

library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)


#01 collecting the data

data("AssociatedPress")

AssociatedPress

#02 calling the LDA
ap_lda_exam <- LDA(AssociatedPress, k=2, control=list(seed=123)) 
ap_lda_exam

#03 per topic creation using tidy 
ap_topics_exam <- tidy(ap_lda_exam, matrix="beta")
ap_topics_exam

top_terms_exam <- ap_topics_exam %>%
  group_by(topic) %>%
  top_n(20,000, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms_exam

top_terms_exam %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + #term = "x" variable and beta = "y variable"
  geom_col(show.legend = FALSE) + #Creating Bar Charts
  facet_wrap(~topic, scales = "free") + #Creating divisions depending on variables after the "~" sign
  coord_flip()

beta_spread_exam <- ap_topics_exam %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.0000005 | topic2 >0.00000005) %>%
  mutate(log_rate = log2(topic1/topic2))

beta_spread_exam

#The more positive the log_rate is = the strongest word related to Topic 1, while the more negative the log_rate is = the strongest word related to Topic 2#

#positive = topic 1
#negative = topic 2

#####################################
##########Question 4

library(dplyr)
library(magrittr)
library(tidytext)
library(tidyverse)
library(gutenbergr)
library(tidyr)

data(stop_words)

#01 download docs
gutenberg_group_1 <- gutenberg_download(c(100,101), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_exam_g1 <- gutenberg_group_1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_exam_g1)
#counting frequencies for tokens
tidy_exam_g1 %>%
  count(word, sort=TRUE)


gutenberg_group_2 <- gutenberg_download(c(110,111), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_exam_g2 <- gutenberg_group_2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_exam_g2)
#counting frequencies for tokens
tidy_exam_g2 %>%
  count(word, sort=TRUE)

gutenberg_group_3 <- gutenberg_download(c(120,121), mirror = "http://mirrors.xmission.com/gutenberg/")
tidy_exam_g3 <- gutenberg_group_3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_exam_g3)
#counting frequencies for tokens
tidy_exam_g3 %>%
  count(word, sort=TRUE)



#02 combining into one frequency
frequency_exam <- bind_rows(mutate(tidy_exam_g1, author="gutenberg_group_1"),
                       mutate(tidy_exam_g2, author= "gutenberg_group_2"),
                       mutate(tidy_exam_g3, author="gutenberg_group_3")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `gutenberg_group_1`, `gutenberg_group_2`)


#03 Corr Test for correlation coefficients 
cor.test(data=frequency_exam[frequency_exam$author == "gutenberg_group_1",],
         ~proportion + `gutenberg_group_3`)

cor.test(data=frequency_exam[frequency_exam$author == "gutenberg_group_2",],
         ~proportion + `gutenberg_group_3`)







