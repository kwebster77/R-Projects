####################################
############Hult International Business School 
############Team 11
############ TA & NLP // Data Visualization 
############Canada Comparison
############ 12/14/2021
###################################

print(airbnb_all$host$host_location)

##calling all libraries
library (mongolite)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

##Karley's Access: 
connection_string <- MongoDb access


######Cleaning the data#########  
#######By Language 

airbnb_all$language <- detect_language(airbnb_all$description)

airbnb_all <- subset(airbnb_all, language == "en")

######Analyzing description filtered by property type#########  
####### creating a tidy format for Apartment
head(airbnb_all$host$host_location)

apt_canada <- airbnb_all%>%
  filter(property_type== "Apartment") %>% 
  filter(host$host_location == "Montreal, Quebec, Canada") %>%
  filter(review_scores$review_scores_rating < 90 | review_scores$review_scores_rating > 99)


tidy_apt_canada <- apt_canada %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_apt_canada) 

####### creating a tidy format for House
house_canada <- airbnb_all %>%
  filter(property_type== "House")%>% 
  filter(host$host_location == "Montreal, Quebec, Canada") %>%
  filter(review_scores$review_scores_rating < 90 | review_scores$review_scores_rating > 99)


tidy_house_canada <- house_canada %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_house_canada)

####### creating a tidy format for Condominium
condo_canada <- airbnb_all %>%
  filter(property_type== "Condominium") %>% 
  filter(host$host_location == "Montreal, Quebec, Canada") %>%
  filter(review_scores$review_scores_rating < 90 | review_scores$review_scores_rating > 99)


tidy_condo_canada <- condo_canada %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_condo_canada)

####### creating a tidy format for Serviced Apartment
servapt_canada <- airbnb_all %>%
  filter(property_type== "Serviced apartment") %>% 
  filter(host$host_location == "Montreal, Quebec, Canada") %>%
  filter(review_scores$review_scores_rating < 90 | review_scores$review_scores_rating > 99)


tidy_servapt_canada <- servapt_canada %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_servapt_canada)


####### creating a tidy format for Loft
loft_canada <- airbnb_all %>%
  filter(property_type== "Loft") %>% 
  filter(host$host_location == "Montreal, Quebec, Canada") %>%
  filter(review_scores$review_scores_rating < 90 | review_scores$review_scores_rating > 99)


tidy_loft_canada <- loft_canada %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_loft_canada)  

####Combining all data sets and doing frequencies 
####Combining all data sets and doing frequencies 
frequency_canada <- bind_rows(mutate(tidy_house_canada, author="House"),
                           mutate(tidy_condo_canada, author= "Condominium"),
                           mutate(tidy_apt_canada, author="Apartment"),
                           mutate(tidy_servapt_canada, author="Serviced apartment"),
                           mutate(tidy_loft_canada, author="Loft")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>% #
  spread(author, proportion) %>% 
  gather(author, proportion, `Condominium`, `Apartment`,`Serviced apartment`,`Loft`) %>% 
  na.omit()

###plotting correlograms----
ggplot(frequency_canada, aes(x=proportion, y=`House`, 
                          color = abs(`House`- proportion)))+
  geom_abline(color="gray40", lty=2)+
  geom_jitter(alpha=0.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "House", x=NULL)

####doing the cor.test() 
cor.test(data=frequency_canada[frequency_canada$author == "Condominium",],
         ~proportion + `House`)

cor.test(data=frequency_canada[frequency_canada$author == "Apartment",],
         ~proportion + `House`)

cor.test(data=frequency_canada[frequency_canada$author == "Serviced apartment",],
         ~proportion + `House`)

cor.test(data=frequency_canada[frequency_canada$author == "Loft",],
         ~proportion + `House`)


###Doing sentiment analysis----
#Most common positive and negative words for apartment  
bing_counts_apt_canada <- tidy_apt_canada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apt_canada

bing_counts_apt_canada %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartment in Canada", x=NULL)+
  coord_flip()

# Most common positive and negative words for house 
bing_counts_house_canada <- tidy_house_canada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house_canada

bing_counts_house_canada %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentimentfor house in Canada", x=NULL)+
  coord_flip()  


#Most common positive and negative words for condominium
bing_counts_condo_canada <- tidy_condo_canada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo_canada

bing_counts_condo_canada %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for condominium in Canada", x=NULL)+
  coord_flip()

#Most common positive and negative words for apartment  
bing_counts_servapt_canada <- tidy_servapt_canada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_servapt_canada

bing_counts_servapt_canada %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for serviced apartment in Canada", x=NULL)+
  coord_flip()

#Most common positive and negative words for Loft
bing_counts_loft_canada <- tidy_loft_canada %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_loft_canada

bing_counts_loft_canada %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for loft in canada", x=NULL)+
  coord_flip()



