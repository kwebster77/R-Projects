####################################
############Hult International Business School 
############Team 11
############ TA & NLP // Data Visualization 
############lower and Lower Bounds
############ 12/14/2021
###################################

##installing packages 
url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
pkgFile<-"cldr_1.1.0.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
unlink(pkgFile)

#install.packages("cld3")
#install.packages("cld2")
#install.packages("tm") # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator
#install.packages("wordcloud2") # word-cloud generator
#install.packages("RColorBrewer") # color palettes


if (!require("pacman")) install.packages("pacman") # for package manangement
pacman::p_load("tidyverse") 
pacman::p_load("textcat")
pacman::p_load("cld2")
pacman::p_load("cld3")
pacman::p_load("rtweet")


##calling all libraries
library (mongolite)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(cld2)
library(cld3)
library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")


##Karley's Access: 
connection_string <- MongoDb access

######Cleaning the data#########  
#######By Language 

airbnb_all$language <- detect_language(airbnb_all$description)

airbnb_all <- subset(airbnb_all, language == "en")

#######filter by top and bottom quantile 
sapply(airbnb_all$review_scores, function(x) sum(is.na(x)))
review_score <- airbnb_all[-which(is.na(airbnb_all$review_scores$review_scores_rating)),]
quantile(review_score$review_scores$review_scores_rating, 0.25)
quantile(review_score$review_scores$review_scores_rating, 0.75)
cleaned_review <- subset(review_score$review_scores, review_scores_rating < 90 | review_scores_rating > 99)

#############################################3
##Upper bound only 

#######filter by top quantile 
sapply(airbnb_all$review_scores, function(x) sum(is.na(x)))
review_score <- airbnb_all[-which(is.na(airbnb_all$review_scores$review_scores_rating)),]
quantile(review_score$review_scores$review_scores_rating, 0.25)
quantile(review_score$review_scores$review_scores_rating, 0.75)
cleaned_review_upper <- subset(review_score$review_scores, review_scores_rating > 99)


######Analyzing description filtered by property type#########  
####### creating a tidy format for Apartment
apt_upper <- airbnb_all%>%
  filter(property_type== "Apartment") %>%
  filter(review_scores$review_scores_rating > 99) 

tidy_apt_upper <- apt_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_apt_upper) 

####### creating a tidy format for House
house_upper <- airbnb_all %>%
  filter(property_type== "House") %>%
  filter(review_scores$review_scores_rating > 99) 

tidy_house_upper <- house_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_house_upper)

####### creating a tidy format for Condominium
condo_upper <- airbnb_all %>%
  filter(property_type== "Condominium") %>%
  filter(review_scores$review_scores_rating > 99) 

tidy_condo_upper <- condo_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_condo_upper)

####### creating a tidy format for Serviced Apartment
servapt_upper <- airbnb_all %>%
  filter(property_type== "Serviced apartment") %>%
  filter(review_scores$review_scores_rating > 99) 

tidy_servapt_upper <- servapt_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_servapt_upper)


####### creating a tidy format for Loft
loft_upper <- airbnb_all %>%
  filter(property_type== "Loft") %>%
  filter(review_scores$review_scores_rating > 99) 

tidy_loft_upper <- loft_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_loft_upper)  

####Combining all data sets and doing frequencies 
####Combining all data sets and doing frequencies 
frequency_upper <- bind_rows(mutate(tidy_house_upper, author="House"),
                             mutate(tidy_condo_upper, author= "Condominium"),
                             mutate(tidy_apt_upper, author="Apartment"),
                             mutate(tidy_servapt_upper, author="Serviced apartment"),
                             mutate(tidy_loft_upper, author="Loft")
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
ggplot(frequency_upper, aes(x=proportion, y=`House`, 
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
cor.test(data=frequency_upper[frequency_upper$author == "Condominium",],
         ~proportion + `House`)

cor.test(data=frequency_upper[frequency_upper$author == "Apartment",],
         ~proportion + `House`)

cor.test(data=frequency_upper[frequency_upper$author == "Serviced apartment",],
         ~proportion + `House`)

cor.test(data=frequency_upper[frequency_upper$author == "Loft",],
         ~proportion + `House`)


###Doing sentiment analysis----
#Most common positive and negative words for apartment  
bing_counts_apt_upper <- tidy_apt_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apt_upper

bing_counts_apt_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartment with high ratings", x=NULL)+
  coord_flip()

# Most common positive and negative words for house 
bing_counts_house_upper <- tidy_house_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house_upper

bing_counts_house_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for house with high ratings", x=NULL)+
  coord_flip()  


#Most common positive and negative words for condominium
bing_counts_condo_upper <- tidy_condo_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo_upper

bing_counts_condo_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for condominium with high ratings", x=NULL)+
  coord_flip()

#Most common positive and negative words for Serviced Apartment 
bing_counts_servapt_upper <- tidy_servapt_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_servapt_upper

bing_counts_servapt_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for serviced apartment with high ratings", x=NULL)+
  coord_flip()

#Most common positive and negative words for Loft
bing_counts_loft_upper <- tidy_loft_upper %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_loft_upper

bing_counts_loft_upper %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for loft with high ratings", x=NULL)+
  coord_flip()



############################################
###Lower Bound Only 
#############################################3
##Upper bound only 

#######filter by top quantile 
cleaned_review_lower <- subset(review_score$review_scores, review_scores_rating < 90)


######Analyzing description filtered by property type#########  
####### creating a tidy format for Apartment
apt_lower <- airbnb_all%>%
  filter(property_type== "Apartment") %>%
  filter(review_scores$review_scores_rating < 90) 

tidy_apt_lower <- apt_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_apt_lower) 

####### creating a tidy format for House
house_lower <- airbnb_all %>%
  filter(property_type== "House") %>%
  filter(review_scores$review_scores_rating < 90) 

tidy_house_lower <- house_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_house_lower)

####### creating a tidy format for Condominium
condo_lower <- airbnb_all %>%
  filter(property_type== "Condominium") %>%
  filter(review_scores$review_scores_rating < 90) 

tidy_condo_lower <- condo_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_condo_lower)

####### creating a tidy format for Serviced Apartment
servapt_lower <- airbnb_all %>%
  filter(property_type== "Serviced apartment") %>%
  filter(review_scores$review_scores_rating < 90) 

tidy_servapt_lower <- servapt_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_servapt_lower)


####### creating a tidy format for Loft
loft_lower <- airbnb_all %>%
  filter(property_type== "Loft") %>%
  filter(review_scores$review_scores_rating < 90) 

tidy_loft_lower <- loft_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_loft_lower)  

####Combining all data sets and doing frequencies 
####Combining all data sets and doing frequencies 
frequency_lower <- bind_rows(mutate(tidy_house_lower, author="House"),
                             mutate(tidy_condo_lower, author= "Condominium"),
                             mutate(tidy_apt_lower, author="Apartment"),
                             mutate(tidy_servapt_lower, author="Serviced apartment"),
                             mutate(tidy_loft_lower, author="Loft")
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
ggplot(frequency_lower, aes(x=proportion, y=`House`, 
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
cor.test(data=frequency_lower[frequency_lower$author == "Condominium",],
         ~proportion + `House`)

cor.test(data=frequency_lower[frequency_lower$author == "Apartment",],
         ~proportion + `House`)

cor.test(data=frequency_lower[frequency_lower$author == "Serviced apartment",],
         ~proportion + `House`)

cor.test(data=frequency_lower[frequency_lower$author == "Loft",],
         ~proportion + `House`)


###Doing sentiment analysis----
#Most common positive and negative words for apartment  
bing_counts_apt_lower <- tidy_apt_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apt_lower

bing_counts_apt_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for apartment with low ratings", x=NULL)+
  coord_flip()

# Most common positive and negative words for house 
bing_counts_house_lower <- tidy_house_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house_lower

bing_counts_house_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for house with low ratings", x=NULL)+
  coord_flip()  


#Most common positive and negative words for condominium
bing_counts_condo_lower <- tidy_condo_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo_lower

bing_counts_condo_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for condominium with low ratings", x=NULL)+
  coord_flip()

#Most common positive and negative words for Serviced Apartment 
bing_counts_servapt_lower <- tidy_servapt_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_servapt_lower

bing_counts_servapt_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for serviced apartment with low ratings", x=NULL)+
  coord_flip()

#Most common positive and negative words for Loft
bing_counts_loft_lower <- tidy_loft_lower %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_loft_lower

bing_counts_loft_lower %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment for loft wth low ratings", x=NULL)+
  coord_flip()


#####################################################
###word cloud for upper bound words-- all house types 

##Unnest tokens for all keywords
all_upper <- airbnb_all%>%
  filter(review_scores$review_scores_rating > 99) 

tidy_all_upper <- all_upper %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_all_upper) 


#Create a vector containing only the text of KEYWORD
data_Keywords <-as.data.frame(tidy_all_upper$word)  
#create a corpus
clue <- Corpus(VectorSource(data_Keywords))
#cleaning rows:
data_Keywords<-gsub("\\[","",data_Keywords)
data_Keywords<-gsub("\\]","",data_Keywords)
clue <- clue %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
clue <- tm_map(clue, content_transformer(tolower))
#create document term-matrix
dtm <- TermDocumentMatrix(clue)
matrix_keyword <- as.matrix(dtm)
words <- sort(rowSums(matrix_keyword), decreasing=TRUE)
df_keyword <- data.frame(word = names(words), freq = words)

wordcloud(words = df_keyword$word, freq = df_keyword$freq, min.freq =
            75,max.words=Inf,
          random.order=TRUE,rot.per=.1,
          colors=brewer.pal(7, "Dark2"))


##lower bound word cloud 

##Unnest tokens for all keywords
all_lower <- airbnb_all%>%
  filter(review_scores$review_scores_rating < 90) 

tidy_all_lower <- all_lower %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
print(tidy_all_lower) 


#Create a vector containing only the text of KEYWORD
data_Keywords_lower <-as.data.frame(tidy_all_lower$word)  
#create a corpus
clue_lower <- Corpus(VectorSource(data_Keywords_lower))
#cleaning rows:
data_Keywords_lower<-gsub("\\[","",data_Keywords_lower)
data_Keywords_lower<-gsub("\\]","",data_Keywords_lower)
clue_lower <- clue_lower %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
clue_lower <- tm_map(clue_lower, content_transformer(tolower))
#create document term-matrix
dtm_lower <- TermDocumentMatrix(clue_lower)
matrix_keyword_lower <- as.matrix(dtm_lower)
words_lower <- sort(rowSums(matrix_keyword_lower), decreasing=TRUE)
df_keyword_lower <- data.frame(word = names(words_lower), freq = words_lower)

wordcloud(words = df_keyword_lower$word, freq = df_keyword_lower$freq, min.freq =
            75,max.words=Inf,
          random.order=TRUE,rot.per=.1,
          colors=brewer.pal(7, "Dark2"))

##comparing word frequency for upper and lower 
##upper

freq_hist_upper <- tidy_all_upper %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip() 

print(freq_hist_upper)

##lower

freq_hist_lower <- tidy_all_lower%>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip() 

print(freq_hist_lower)

