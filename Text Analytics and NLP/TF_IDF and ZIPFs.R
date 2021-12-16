##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster
##### MBAN2 HULT 2021
##### TF_IDF and ZIPFs
##### Version 0.1

library(dplyr)
library(stringr)
library(tidytext)

#let's look at the data
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

#we're grouping by the country this time
netflix_token <- netflix %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- netflix_token %>%
  group_by(country) %>%
  summarize(total=sum(n))

netflix_words <- left_join(netflix_token, total_words)%>%
  filter(country %in% c("United States", "Mexico", "India"))

print(netflix_words)

library(ggplot2)
ggplot(netflix_words, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) + #Not exceed a determine value. Two functions that can be used in such situations are the ylim() and xlim() functions. Both these functions are used to set the lower and upper limit on the y-axis and x-axis
  facet_wrap(~country, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 

########## ZIPF's law

freq_by_rank <- netflix_words %>%
  group_by(country) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=country))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=3)+ #creating the slope of a grpah
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

################# TF_IDF

country_words <- netflix_words %>%
  bind_tf_idf(word, country, n)

country_words # we get all the zeors because we are looking at stop words ... too common

country_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

# looking at the graphical apprach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()



