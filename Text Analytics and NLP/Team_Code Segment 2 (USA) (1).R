####################################
############Hult International Business School 
############Team 11
############ TA & NLP // Data Visualization 
############usa_ lower and upper comparison - Lower bound and Upper Bound
############ 12/14/2021
###################################

##calling all libraries
library (mongolite)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

##Karley's Access: 
connection_string <- 'mongodb+srv://team11:team11@cluster0.zefbg.mongodb.net/Cluster0?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
airbnb_all <- airbnb_collection$find()
as.data.frame(airbnb_all)

######Cleaning the data#########  
#######By Language 

airbnb_all$language <- detect_language(airbnb_all$description)

airbnb_all <- subset(airbnb_all, language == "en")

######################################
##Lower bound only

######Analyzing description filtered by property type#########  
####### creating a tidy format for Apartment
head(airbnb_all$host$host_location)

keep <- c("US", "United States", "usa", "USA")

apt_usa_lower <- airbnb_all%>%
  filter(property_type== "Apartment") %>% 
  filter(review_scores$review_scores_rating < 90) 

tidy_apt_usa_lower <- apt_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_apt_usa_lower <- tidy_apt_usa_lower[grepl(paste(keep, collapse='|'), tidy_apt_usa_lower$host$host_location),]
print(tidy_apt_usa_lower) 

####### creating a tidy format for House
house_usa_lower <- airbnb_all %>%
  filter(property_type== "House")%>% 
  filter(review_scores$review_scores_rating < 90) 


tidy_house_usa_lower <- house_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_house_usa_lower <- tidy_house_usa_lower[grepl(paste(keep, collapse='|'), tidy_house_usa_lower$host$host_location),]
print(tidy_house_usa_lower)

####### creating a tidy format for Condominium
condo_usa_lower <- airbnb_all %>%
  filter(property_type== "Condominium") %>% 
  filter(review_scores$review_scores_rating < 90) 


tidy_condo_usa_lower <- condo_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_condo_usa_lower <- tidy_condo_usa_lower[grepl(paste(keep, collapse='|'), tidy_condo_usa_lower$host$host_location),]
print(tidy_condo_usa_lower)

####### creating a tidy format for Serviced Apartment
servapt_usa_lower <- airbnb_all %>%
  filter(property_type== "Serviced apartment") %>% 
  filter(review_scores$review_scores_rating < 90) 


tidy_servapt_usa_lower <- servapt_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_servapt_usa_lower <- tidy_servapt_usa_lower[grepl(paste(keep, collapse='|'), tidy_servapt_usa_lower$host$host_location),]
print(tidy_servapt_usa_lower)


####### creating a tidy format for Loft
loft_usa_lower <- airbnb_all %>%
  filter(property_type== "Loft") %>% 
  filter(review_scores$review_scores_rating < 90) 


tidy_loft_usa_lower <- loft_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_loft_usa_lower <- tidy_loft_usa_lower[grepl(paste(keep, collapse='|'), tidy_loft_usa_lower$host$host_location),]
print(tidy_loft_usa_lower)  

####Combining all data sets and doing frequencies 
####Combining all data sets and doing frequencies 
frequency_usa_lower <- bind_rows(mutate(tidy_house_usa_lower, author="House"),
                           mutate(tidy_condo_usa_lower, author= "Condominium"),
                           mutate(tidy_apt_usa_lower, author="Apartment"),
                           mutate(tidy_apt_usa_lower, author="Serviced Apartment"),
                           mutate(tidy_loft_usa_lower, author="Loft")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>% #
  spread(author, proportion) %>% 
  gather(author, proportion, `Condominium`, `Apartment`, `Serviced Apartment`, `Loft`) %>% 
  na.omit()

###plotting correlograms----
ggplot(frequency_usa_lower, aes(x=proportion, y=`House`, 
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
cor.test(data=frequency_usa_lower[frequency_usa_lower$author == "Condominium",],
         ~proportion + `House`)

cor.test(data=frequency_usa_lower[frequency_usa_lower$author == "Apartment",],
         ~proportion + `House`)

cor.test(data=frequency_usa_lower[frequency_usa_lower$author == "Loft",],
         ~proportion + `House`)

cor.test(data=frequency_usa_lower[frequency_usa_lower$author == "Serviced Apartment",],
         ~proportion + `House`)

###Doing sentiment analysis----
#Most common positive and negative words for apartment  
bing_counts_apt_usa_lower <- tidy_apt_usa_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apt_usa_lower

bing_counts_apt_usa_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartments in US", x=NULL)+
  coord_flip()

# Most common positive and negative words for house 
bing_counts_house_usa_lower <- tidy_house_usa_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house_usa_lower

bing_counts_house_usa_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()  


#Most common positive and negative words for condominium
bing_counts_condo_usa_lower <- tidy_condo_usa_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo_usa_lower

bing_counts_condo_usa_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for condominiums in US", x=NULL)+
  coord_flip()

#Most common positive and negative words for Loft
bing_counts_loft_usa_lower <- tidy_loft_usa_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_loft_usa_lower

bing_counts_loft_usa_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Loft in US", x=NULL)+
  coord_flip()

######################################
##Upper bound only

######Analyzing description filtered by property type#########  
####### creating a tidy format for Apartment
head(airbnb_all$host$host_location)

keep <- c("US", "United States", "usa", "USA")

apt_usa_upper <- airbnb_all%>%
  filter(property_type== "Apartment") %>% 
  filter(review_scores$review_scores_rating > 99) 

tidy_apt_usa_upper <- apt_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_apt_usa_upper <- tidy_apt_usa_upper[grepl(paste(keep, collapse='|'), tidy_apt_usa_upper$host$host_location),]
print(tidy_apt_usa_upper) 

####### creating a tidy format for House
house_usa_upper <- airbnb_all %>%
  filter(property_type== "House")%>% 
  filter(review_scores$review_scores_rating > 99) 


tidy_house_usa_upper <- house_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_house_usa_upper <- tidy_house_usa_upper[grepl(paste(keep, collapse='|'), tidy_house_usa_upper$host$host_location),]
print(tidy_house_usa_upper)

####### creating a tidy format for Condominium
condo_usa_upper <- airbnb_all %>%
  filter(property_type== "Condominium") %>% 
  filter(review_scores$review_scores_rating > 99) 


tidy_condo_usa_upper <- condo_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

tidy_condo_usa_upper <- tidy_condo_usa_upper[grepl(paste(keep, collapse='|'), tidy_condo_usa_upper$host$host_location),]
print(tidy_condo_usa_upper)

####### creating a tidy format for Serviced Apartment
servapt_usa_upper <- airbnb_all %>%
  filter(property_type== "Serviced apartment") %>% 
  filter(review_scores$review_scores_rating > 99) 


tidy_servapt_usa_upper <- servapt_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_servapt_usa_upper <- tidy_servapt_usa_upper[grepl(paste(keep, collapse='|'), tidy_servapt_usa_upper$host$host_location),]
print(tidy_servapt_usa_upper)


####### creating a tidy format for Loft
loft_usa_upper <- airbnb_all %>%
  filter(property_type== "Loft") %>% 
  filter(review_scores$review_scores_rating > 99) 


tidy_loft_usa_upper <- loft_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_loft_usa_upper <- tidy_loft_usa_upper[grepl(paste(keep, collapse='|'), tidy_loft_usa_upper$host$host_location),]
print(tidy_loft_usa_upper)  

####Combining all data sets and doing frequencies 
####Combining all data sets and doing frequencies 
frequency_usa_upper <- bind_rows(mutate(tidy_house_usa_upper, author="House"),
                                 mutate(tidy_condo_usa_upper, author= "Condominium"),
                                 mutate(tidy_apt_usa_upper, author="Apartment"),
                                 mutate(tidy_loft_usa_upper, author="Loft")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n/sum(n))%>% 
  select(-n) %>% #
  spread(author, proportion) %>% 
  gather(author, proportion, `Condominium`, `Apartment`,`Loft`) %>% 
  na.omit()

###plotting correlograms----
ggplot(frequency_usa_upper, aes(x=proportion, y=`House`, 
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
cor.test(data=frequency_usa_upper[frequency_usa_upper$author == "Condominium",],
         ~proportion + `House`)

cor.test(data=frequency_usa_upper[frequency_usa_upper$author == "Apartment",],
         ~proportion + `House`)

cor.test(data=frequency_usa_upper[frequency_usa_upper$author == "Loft",],
         ~proportion + `House`)


###Doing sentiment analysis----
#Most common positive and negative words for apartment  
bing_counts_apt_usa_upper <- tidy_apt_usa_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apt_usa_upper

bing_counts_apt_usa_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartments in US", x=NULL)+
  coord_flip()

# Most common positive and negative words for house 
bing_counts_house_usa_upper <- tidy_house_usa_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house_usa_upper

bing_counts_house_usa_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()  


#Most common positive and negative words for condominium
bing_counts_condo_usa_upper <- tidy_condo_usa_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo_usa_upper

bing_counts_condo_usa_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for condominiums in US", x=NULL)+
  coord_flip()

#Most common positive and negative words for Loft
bing_counts_loft_usa_upper <- tidy_loft_usa_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_loft_usa_upper

bing_counts_loft_usa_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for Loft in US", x=NULL)+
  coord_flip()

##Unnest tokens for all keywords
all_usa_upper <- airbnb_all%>%
  filter(review_scores$review_scores_rating > 99) 

tidy_all_usa_upper <- all_usa_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_all_usa_upper <- tidy_all_usa_upper[grepl(paste(keep, collapse='|'), tidy_all_usa_upper$host$host_location),]
print(tidy_all_usa_upper) 


#Create a vector containing only the text of KEYWORD
data_Keywords_usa_upper <-as.data.frame(tidy_all_usa_upper$word)  
#create a corpus
clue_usa_upper <- Corpus(VectorSource(data_Keywords_usa_upper))
#cleaning rows:
data_Keywords_usa_upper<-gsub("\\[","",data_Keywords_usa_upper)
data_Keywords_usa_upper<-gsub("\\]","",data_Keywords_usa_upper)
clue_usa_upper <- clue_usa_upper %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
clue_usa_upper <- tm_map(clue, content_transformer(tolower))
#create document term-matrix
dtm_usa_upper <- TermDocumentMatrix(clue_usa_upper)
matrix_keyword_usa_upper <- as.matrix(dtm_usa_upper)
words_usa_upper <- sort(rowSums(matrix_keyword_usa_upper), decreasing=TRUE)
df_keyword_usa_upper <- data.frame(word = names(words_usa_upper), freq = words_usa_upper)

wordcloud(words = df_keyword_usa_upper$word, freq = df_keyword_usa_upper$freq, min.freq =
            75,max.words=Inf,
          random.order=TRUE,rot.per=.1,
          colors=brewer.pal(7, "Dark2"))


##lower bound word cloud 

##Unnest tokens for all keywords
all_usa_lower <- airbnb_all%>%
  filter(review_scores$review_scores_rating < 90) 

tidy_all_usa_lower <- all_usa_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
tidy_all_usa_lower <- tidy_all_usa_lower[grepl(paste(keep, collapse='|'), tidy_all_usa_lower$host$host_location),]
print(tidy_all_usa_lower) 


#Create a vector containing only the text of KEYWORD
data_Keywords_usa_lower <-as.data.frame(tidy_all_usa_lower$word)  
#create a corpus
clue_usa_lower <- Corpus(VectorSource(data_Keywords_usa_lower))
#cleaning rows:
data_Keywords_usa_lower<-gsub("\\[","",data_Keywords_usa_lower)
data_Keywords_usa_lower<-gsub("\\]","",data_Keywords_usa_lower)
clue_usa_lower <- clue_usa_lower %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
clue_usa_lower <- tm_map(clue_usa_lower, content_transformer(tolower))
#create document term-matrix
dtm_usa_lower <- TermDocumentMatrix(clue_usa_lower)
matrix_keyword_usa_lower <- as.matrix(dtm_usa_lower)
words_usa_lower <- sort(rowSums(matrix_keyword_usa_lower), decreasing=TRUE)
df_keyword_usa_lower <- data.frame(word = names(words_usa_lower), freq = words_usa_lower)

wordcloud(words = df_keyword_usa_lower$word, freq = df_keyword_usa_lower$freq, min.freq =
            75,max.words=Inf,
          random.order=TRUE,rot.per=.1,
          colors=brewer.pal(7, "Dark2"))
