####################################
############Hult International Business School 
############MSBAN Team 7
############ Air France Business Case Team Assignment
############ 11/08/2021
###################################

##Import the libarary and Air france spreadsheet into R
library(readxl)
Air_France_Case_Spreadsheet_Supplement <- read_excel("Documents/Education/Data Science- R/Team Project/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")

##Useful packages which could be used in the air france business case 
#install.packages("plotly")
#install.packages('rJava')
#install.packages("openNLP")
#install.packages("qdap")
#install.packages("dendextend")
#install.packages("ggthemes")
#install.packages("RWeka")

##packages have been installed- the libraries need to run

library(dplyr)
library(plotly)
library(ggplot2)
library(plotrix)
library(rpart)

####################################################################################################################

1. # Understanding data - Air France
#Find the number of rows in the data set
nrow(Air_France_Case_Spreadsheet_Supplement)

####################################################################################################################

2. # Massaging data
# checking fields that to be used
is.character(Air_France_Case_Spreadsheet_Supplement$`Publisher Name`)
is.character(Air_France_Case_Spreadsheet_Supplement$Keyword)
is.numeric(Air_France_Case_Spreadsheet_Supplement$Clicks)
is.numeric(Air_France_Case_Spreadsheet_Supplement$Impressions)
is.numeric(Air_France_Case_Spreadsheet_Supplement$Amount)
is.numeric(Air_France_Case_Spreadsheet_Supplement$`Total Cost`)
is.numeric(Air_France_Case_Spreadsheet_Supplement$`Total Volume of Bookings`)

#check for missing values 
sapply(Air_France_Case_Spreadsheet_Supplement, function(x) sum(is.na(x)))

#blanks are in Bid strategy-- all others do not have blanks
Air_France_Case_Spreadsheet_Supplement$`Bid Strategy` [which(is.na(Air_France_Case_Spreadsheet_Supplement$`Bid Strategy`))]<- "NA"

#re-run to check for missing values 
sapply(Air_France_Case_Spreadsheet_Supplement, function(x) sum(is.na(x)))

#taking out rows with total cost of zero 
Air_France_Case_Spreadsheet_Supplement_no_zero <- Air_France_Case_Spreadsheet_Supplement[-which(Air_France_Case_Spreadsheet_Supplement$`Total Cost` == 0),]

####################################################################################################################

3. #Descriptive statistics 

#creating summary of the data set
summary(Air_France_Case_Spreadsheet_Supplement)

#rename for easier manipulation 
af_case <- Air_France_Case_Spreadsheet_Supplement_no_zero

#ROA
af_case$ROA <- af_case$Amount/af_case$`Total Cost`

#cost per booking 
af_case$'Cost Per Booking' <- af_case$`Total Cost`/af_case$`Total Volume of Bookings`

#cleaning infinity values 
af_case$`Cost Per Booking`[which(af_case$`Cost Per Booking`=="Inf")] <- 0

################################################################################################################
####################           AGGREGATION               #######################################################
################################################################################################################

#create a summary of KPIs per publisher
agt_amount <- as.data.frame(aggregate(af_case$Amount,
                                      list(af_case$`Publisher Name`), FUN = sum))

names(agt_amount)

names(agt_amount)[1] <- "publisher"
names(agt_amount)[2] <- "totalamt"


agt_clicks <- as.data.frame(aggregate(af_case$Clicks,
                                      list(af_case$`Publisher Name`), FUN = sum))
names(agt_clicks)[2] <- "totalclicks"

agt_imprsn <- as.data.frame(aggregate(af_case$Impressions,
                                      list(af_case$`Publisher Name`), FUN = sum))

names(agt_imprsn)[2] <- "totalimprsn"

agt_cost <- as.data.frame(aggregate(af_case$`Total Cost`,
                                    list(af_case$`Publisher Name`), FUN = sum))
names(agt_cost)[2] <- "totalcost"

agt_vol <- as.data.frame(aggregate(af_case$`Total Volume of Bookings`,
                                   list(af_case$`Publisher Name`), FUN = sum))
names(agt_vol)[2] <- "totalvol"

agt_bid <- as.data.frame(aggregate(af_case$`Search Engine Bid`,
                                   list(af_case$`Publisher Name`), FUN = sum))
names(agt_bid)[2] <- "totalbid"


# Combining all the aggregated variables and creating a publisher wise new data set excluding repeated columns

air_france_cbn <- cbind(agt_amount,agt_clicks,agt_cost,agt_imprsn,agt_vol, agt_bid)
air_france_new <- subset(air_france_cbn, select = c(publisher, totalamt, totalclicks, totalcost, totalimprsn, totalvol, totalbid)) 
air_france_new$ROA <- air_france_new$totalamt/air_france_new$totalcost

# Creating a summary of the new data set
summary(air_france_new)

#################################################################################################################

# Creating a for loop to identify top values in each column along with the publisher name and column name

for (i in 2:ncol(air_france_new)) {
  if(max(air_france_new[,i])) {
    print(c(colnames(air_france_new)[i],
            air_france_new[which.max(air_france_new[,i]), 1],
            max(air_france_new[,i])))
  }
}

# Creating a for loop to identify lowest values in each column along with the publisher name and column name

for (i in 2:ncol(air_france_new)) {
  if(min(air_france_new[,i])) {
    print(c(colnames(air_france_new)[i],
            air_france_new[which.min(air_france_new[,i]), 1],
            min(air_france_new[,i])))
  }
  else{
    print(c(colnames(air_france_new)[i], air_france_new[which(air_france_new$totalbid == 0), 1]))
  }
}

################################################        VISUALISATION             ############################# 

#bar chart to compare ROA for each publisher

Fig_1 <- plot_ly(air_france_new, x=air_france_new$publisher, y=air_france_new$ROA,type="bar", marker = list(color = c("silver",
                                                                                    "silver",
                                                                                    "silver",
                                                                                    "silver",
                                                                                    "silver",
                                                                                    "silver",
                                                                                    "red"))) %>% layout(title = "Return on Advertising")


print(Fig_1)

#bar chart for comparing click through rate across publishers

Fig_2 <- plot_ly(air_france_new, x=air_france_new$publisher, y=air_france_new$totalclicks/air_france_new$totalimprsn, type = "bar", marker = list(color = c("silver",
                                                                                                                                            "silver",
                                                                                                                                            "red",
                                                                                                                                            "silver",
                                                                                                                                            "silver",
                                                                                                                                            "silver",
                                                                                                                                            "silver"))) %>% layout(title = "Click Through Rate")

print(Fig_2)

#bar chart for comparing conversions 

Fig_3 <- plot_ly(air_france_new, x=air_france_new$publisher, y=air_france_new$totalvol/air_france_new$totalclicks, type = "bar", marker = list(color = c("silver",
                                                                                                                                                            "silver",
                                                                                                                                                            "silver",
                                                                                                                                                            "silver",
                                                                                                                                                            "silver",
                                                                                                                                                            "silver",
                                                                                                                                                            "red"))) %>% layout(title = "Conversion Rate")

print(Fig_3)

#Pie chart for publisher wise for overall revenue

total_rev <- sum(af_case$Amount)
yahoo_US_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="Yahoo - US"])
MSN_glob_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="MSN - Global"])
Google_glob_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="Google - Global"])
Overture_glob_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="Overture - Global"])
Google_US_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="Google - US"])
Overture_US_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="Overture - US"])
MSN_US_rev <- sum(af_case$Amount[af_case$`Publisher Name`=="MSN - US"])

Yahoo_US_rev_chart <- yahoo_US_rev/total_rev
Google_glob_rev_chart <- Google_glob_rev/total_rev
Google_US_rev_chart <- Google_glob_rev/total_rev
Overture_glob_rev_chart <- Overture_glob_rev/total_rev
Overture_US_rev_chart <- Overture_US_rev/total_rev
MSN_glob_rev_chart <- MSN_glob_rev/total_rev
MSN_US_rev_chart <- MSN_US_rev/total_rev

list_rev <- c(Yahoo_US_rev_chart,Google_US_rev_chart,Google_glob_rev_chart,
              Overture_US_rev_chart,Overture_glob_rev_chart, MSN_US_rev_chart,
              MSN_glob_rev_chart)
label <- c("Yahoo US", "Google US", "Google Global", "Overture US", "Overture Global", 
           "MSN US", "MSN Global")
Rev_outline_per_publisher <- pie(list_rev,label)

Yahoo_US_rev_chart_conso <- round((yahoo_US_rev/total_rev)*100)
google_rev_chart <- round(((Google_US_rev+Google_glob_rev)/total_rev)*100)
overture_rev_chart <- round(((Overture_US_rev+Overture_glob_rev)/total_rev)*100)
MSN_rev_chart <- round(((MSN_US_rev+MSN_glob_rev)/total_rev)*100)

total_pub_list_rev <- c(Yahoo_US_rev_chart_conso,google_rev_chart,overture_rev_chart,MSN_rev_chart)
label_total_pub <- c("Yahoo", "Google", "Overture", "MSN")

label_percent <- paste(label_total_pub, total_pub_list_rev,"%")

total_pub_pie_chart <- pie(total_pub_list_rev,label_percent, main = "Share of Total Revenue")

####################################################################################################################

4.# Running regression models
# A. Using click through rate to create a regression model
mean(af_case$`Engine Click Thru %`, na.rm = TRUE)

#for loop to convert Click through rate to  binary
for (i in 1:nrow(af_case)) {
  if (is.na(af_case$`Engine Click Thru %`[i])) {
    af_case$Click_thru_rate_binary[i] <- "0"
    next
  }  
  if (af_case$`Engine Click Thru %`[i] > 11.14) {
    af_case$Click_thru_rate_binary[i] <- "1"
  } else {
    af_case$Click_thru_rate_binary[i] <- "0"
  }
}

af_case$Click_thru_rate_binary <- as.numeric(af_case$Click_thru_rate_binary)

af_logit_clickthru <- glm(Click_thru_rate_binary ~ `Search Engine Bid`+ `Avg. Pos.`+`Avg. Cost per Click`, 
                          data = af_case, family = "binomial")

summary(af_logit_clickthru)

exp(0.07720)-1
exp(-0.77480)-1
exp(0.10764)-1

# B.Using ROA to create a regression model
mean(af_case$ROA)

#for loop to convert ROA to  binary
for (i in 1:nrow(af_case)) {
  if (is.na(af_case$ROA[i])) {
    af_case$ROA_binary[i] <- "0"
    next
  }  
  if (af_case$ROA[i] > 4.415) {
    af_case$ROA_binary[i] <- "1"
  } else {
    af_case$ROA_binary[i] <- "0"
  }
}

af_case$ROA_binary <- as.numeric(af_case$ROA_binary)

af_logit_ROA <- glm(ROA_binary ~ `Search Engine Bid`+ `Avg. Pos.`+ 
                      Clicks + Impressions + `Total Volume of Bookings`, 
                          data = af_case, family = "binomial")

summary(af_logit_ROA)

exp(-0.01669)-1
exp(-0.02558)-1
exp(-0.002072)-1
exp(0.000000948)-1
exp(3.964)-1

# C. Normalized regression model
##UDF for normalizing the data

af_norm_fun <- function(x){
  af_min <- min(x, na.rm=T)
  af_max <- max(x, na.rm = T)
  min_max <- (x-af_min)/(af_max - af_min)
  return(min_max)
}

##normalize the data:

af_norm <- af_case

af_norm$search_bid_norm <- af_norm_fun(x= af_norm$`Search Engine Bid`)
af_norm$Impressions_norm <- af_norm_fun(x= af_norm$Impressions)
af_norm$Clicks_norm <- af_norm_fun(x= af_norm$Clicks)
af_norm$Click_Charges_norm <- af_norm_fun(x= af_norm$`Click Charges`)
af_norm$Trans_Conv_norm <- af_norm_fun(x= af_norm$`Trans. Conv. %`)
af_norm$Total_Volume_of_Bookings <- af_norm_fun(x= af_norm$`Total Volume of Bookings`)
af_norm$Avg_Cost_per_Clicknorm <- af_norm_fun(x= af_norm$`Avg. Cost per Click`)
af_norm$Engine_Click_Thru_norm <- af_norm_fun(x= af_norm$`Engine Click Thru %`)
af_norm$Avg_Pos_norm <- af_norm_fun(x= af_norm$`Avg. Pos.`)
af_norm$Total_Cost_norm <- af_norm_fun(x= af_norm$`Total Cost`)
af_norm$Total_Cost_Trans_norm <- af_norm_fun(x= af_norm$`Total Cost/ Trans.`)
af_norm$Amount_norm <- af_norm_fun(x= af_norm$Amount)
af_norm$impression_binary_norm <- af_norm_fun(x= af_norm$Impressions)

#create a dataframe with just normalized data

af_norm_only <- af_norm[,28:40]


#Creating a new variable as a rating for publishers

af_norm_only$pub_norm_rating <- (0.2*af_norm_only$Engine_Click_Thru_norm+0.3*af_norm_only$Trans_Conv_norm+0.5*af_norm_only$Amount_norm
                            +0.1*af_norm_only$Total_Cost_norm+0.1*af_norm_only$search_bid_norm)

mean(af_norm_only$pub_norm_rating)

# Creating a binary with the normalized model 
for (i in 1:nrow(af_norm_only)) {
  if (is.na(af_norm_only$pub_norm_rating[i])) {
    af_norm_only$pub_norm_rating_binary[i] <- "0"
    next
  }  
  if (af_norm_only$pub_norm_rating[i] > 0.03199) {
    af_norm_only$pub_norm_rating_binary[i] <- "1"
  } else {
    af_norm_only$pub_norm_rating_binary[i] <- "0"
  }
}

af_norm_only$pub_norm_rating_binary <- as.numeric(af_norm_only$pub_norm_rating_binary)

af_norm_only_glm <- glm(pub_norm_rating_binary ~ Engine_Click_Thru_norm+Trans_Conv_norm
                    +search_bid_norm+Total_Cost_norm, data = af_norm_only, family = "binomial")

summary(af_norm_only_glm)


#Creating a binary rating for impressions

summary(af_case$Impressions)

# Creating a binary with the normalized model 
af_norm_only$impressions_binary <- ifelse(af_case$Impressions >9286, 1, 0)
af_norm_only$impressions_binary <- as.numeric(af_norm_only$impressions_binary)
is.numeric(af_norm_only$impressions_binary)

af_norm_only_glm_impressions <- glm(impressions_binary ~ Engine_Click_Thru_norm+Trans_Conv_norm
                                    +search_bid_norm+Total_Cost_norm, data = af_norm_only, family = "binomial")

summary(af_norm_only_glm_impressions)


#creating a confusion Matrix for Click through Rate 

#training and testing dataset 
train_index <- sample(1:nrow(af_case), size=0.8*nrow(af_case))
af_case_train <- af_case[train_index,]
#can create a test environment from subtracting the train environment 
af_case_test <- af_case[-train_index,]

#creating a confusion matrix for testing 
my_prediction_testing <- predict(af_logit_clickthru, af_case_test, type ="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference= as.factor(as.numeric(af_case_test$Click_thru_rate_binary)))

#creating a confusion matrix for training 
my_prediction_training <- predict(af_logit_clickthru, af_case_train, type ="response")
confusionMatrix(data= as.factor(as.numeric(my_prediction_training > 0.5)),
                reference= as.factor(as.numeric(af_case_train$Click_thru_rate_binary)))

#prediction value logistic 
pred_val_logit <- prediction(my_prediction_training, af_case_train$Click_thru_rate_binary)

perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit)


#Gini Decision Tree

my_tree <- rpart(Click_thru_rate_binary ~ `Search Engine Bid`+ `Avg. Pos.`+`Avg. Cost per Click`, 
                 data = af_case, method = "class", cp= 0.003)

rpart.plot(my_tree, extra=1, type=1)


#comparing model performance: 

my_tree_predict_test <- predict(my_tree, af_case_test, type="prob")
my_tree_predict_train <- predict(my_tree, af_case_train, type="prob")

my_tree_prediction <- prediction(my_tree_predict_train[,2], af_case_train$Click_thru_rate_binary)

my_tree_performance <- performance(my_tree_prediction, "tpr", "fpr")

plot(my_tree_performance, col="black")
plot(perf_logit, col="purple", add=TRUE)


####################################################################################################################
############################################### WORD CLOUD ########################################################

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("wordcloud2") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")

#Create a vector containing only the text of KEYWORD
data_Keywords <- af_case$Keyword
#creat a corpus
clue <- Corpus(VectorSource(data_Keywords))

#cleaning rows:
data_Keywords<-gsub("\\[","",data_Keywords)
data_Keywords<-gsub("\\]","",data_Keywords)

clue <- clue %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)
clue <- tm_map(clue, content_transformer(tolower))
clue<- tm_map(clue, removeWords, stopwords("english"))

#create document term-matrix

dtm <- TermDocumentMatrix(clue)
matrix_keyword <- as.matrix(dtm)
words <- sort(rowSums(matrix_keyword), decreasing=TRUE)
df_keyword <- data.frame(word = names(words), freq = words)

set.seed(1234) # for reproducibility 
wordcloud(words = df_keyword$word, freq = df_keyword$freq, min.freq = 1,max.words=300, 
          random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(7, "Dark2"))

###############################################################################################################