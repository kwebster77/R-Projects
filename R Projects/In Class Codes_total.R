#### Calling all the Libraries -------
library("tidytext")
library("tidyverse")
library("tidyr")
library("tidytuesdayR")
library("stringr")
library("textreadr")
library("pdftools")
library("textshape")
library("twitteR")
library("tm")
library("ggplot2")
library("scales")
library("magrittr")
library("dplyr")
library("gutenbergr")
library("Matrix")
library("textdata")
library("igraph")
library("ggraph")
library("widyr")
library("topicmodels")
library("quanteda")
library("quanteda.textmodels")
library("RColorBrewer")

##### Data Science: CLASS 03 - Gsub#####################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 03 - Gsub
##### Date: 14.10.2021
##### Version 0.1

#reading in an excel file from the local drive
library(readxl)
my_germ <- read_excel("Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/03. Data Science/german credit card.xls", 
                                 sheet = "german credit card.csv")
View(my_germ)
#creating a few random objects

count_questions <- 21
count_customers <- 1000

#putting them together in a vector
germ_dim <- c(count_questions, count_customers)
germ_dim <- c(count_customers, count_questions)

#changing the game
customer_age <- c(16, 23, 29, 25, 26, 23, 25, 24, 25, 45)
age_matrix <- matrix(customer_age, nrow=2, ncol=5, byrow=TRUE)

#subsetting
customer_age[1:5]
customer_age[6:10]

my_germ[1:100 , c("age", "amount")]

#creating a list
my_list <- list(my_germ, age_matrix, count_customers, count_questions)

#testing and changing data types
is.numeric(my_germ$good_bad)
is.character(my_germ$good_bad)

#to see frequency table of purpose variable
table(my_germ$purpose)

#testing conversion of vector from one type to another
as.numeric(my_germ$good_bad)
as.numeric(my_germ$purpose)

#using the which() to subset and filter based on business conditions

which(my_germ$purpose == "X")
my_germ[ which(my_germ$purpose == "X"),]

summary(my_germ[ which(my_germ$good_bad == "good"),]) #analyzing good customers
summary(my_germ[ which(my_germ$good_bad == "bad"),])  #analyzing bad customers

###fixing the data with gsub()

#1replacing X in purpose variable with blank space
gsub("X", "", my_germ$purpose) 

#2converting purpose variable to numeric
my_germ$purpose_fixed <- as.numeric(gsub("X", "", my_germ$purpose))

#3replacing good in good_bad variable with 1
my_germ$binary <- gsub("good", "1", my_germ$good_bad) 

#4replacing bad in binary variable with 0
my_germ$binary <- gsub("bad", "0", my_germ$binary) 

#5overwriting binary variable to numeric
my_germ$binary <- as.numeric(my_germ$binary)
print(my_germ)

##### Data Science: CLASS 04 - For Loops#########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 04 - For Loops
##### Date: 19.10.2021
##### Version 0.1

#01 Redefining the data as a data frame because we have been massaging the data
my_germ <- as.data.frame(my_germ)

#02 Creating the for loop for(){}
for(i in 1:3){
  print(mean(my_germ[,i], na.rm=TRUE))
  print(min(my_germ[,i], na.rm=TRUE))
  print(max(my_germ[,i], na.rm=TRUE))
  #na.rm = TRUE is a best coding practice, since it removes NA from the analysis
} #03 closing i loop 

#Creating a new variable
my_germ$score <- c()

#Loop through observations to apply the weighted risk score. 
for(i in 1:nrow(my_germ)){ #By using the nrow we make the loop dinamic. 
  #formula 0.5*duration + 0.1*amount + 0.1*age + 0.3*installp
  my_germ$score[i] <- 0.5*my_germ$duration[i]+
  0.10*my_germ$amount[i]+
    0.10*my_germ$age[i]+
      0.30*my_germ$installp[i]
}#Closing the i-loop
#nrow(my_germ) counts all the rows in the data base



#Creating a new variable for the team evaluation
my_germ$score_team <- c()

for(i in 1:nrow(my_germ)){
  #formula 0.4*duration + 0.10*history + 0.1*amount + 0.2*age + 0.2*employed
  my_germ$score_team[i] <- 0.4*my_germ$duration[i]+
    0.10*my_germ$history[i]+
    0.10*my_germ$amount[i]+
    0.20*my_germ$age[i]+
    0.20*my_germ$employed[i]
}#Closing the i-loop
#01 Labeling based on custom logic using IF statement
my_germ$label <- c()

#02 Creating the For Loop
for (i in 1:nrow(my_germ)){
  #03 Creating the If statement considering a score < 500 and a binary =1 as outstanding
  if (my_germ$score[i]<500 & my_germ$duration[i] == 1) {
    my_germ$label[i] <- "outstanding"
  } else {
  my_germ$label[i] <- "not outstanding"  
  }#Closing the if statement <500 and binary
} #Closing the i loop

##### Data Science: CLASS 05 - User defined functions#########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 05 - User defined functions
##### Date: 22.10.2021
##### Version 0.1

##### User defined functions --> UDFs and desc statistics 

team_score <- function(var1, var2, var3, var4, w1, w2, w3, w4) {#also call function operators
  my_score <- w1*var1 + w2*var2 + w3*var3 + w4*var4
  mean_score <- mean(my_score, na.rm = TRUE)
  min_score <- min(my_score, na.rm = TRUE)
  max_score <- max(my_score, na.rm = TRUE)
  return (c(mean_score, min_score, max_score))
}

#Assigning the UDF team_score to the team10 variables
team10 <- team_score(var1=my_germ$duration, w1=0.4,
                     var2 = my_germ$amount, w2=0.2,
                     var3=my_germ$history, w3=0.2,
                     var4= my_germ$job, w4=0.2)

#Best practice from the previous step
team10best <- team_score(my_germ$duration, 
                         my_germ$amount, 
                         my_germ$history, 
                         my_germ$job, 
                         0.4, 0.2, 0.2, 0.2)

#Assigning the UDF team_score to the team04 variables with best practice
team4_best <- team_score(my_germ$duration,
                         my_germ$amount,
                         my_germ$history,
                         my_germ$employed, 
                         0.3, 0.2, 0.2, 0.3)


#Calculating mu and sigma for all the variables. Looping through the variables
for (i in 1:ncol(my_germ)){
  try(my_mu <- mean(my_germ[,i], na.rm = TRUE)) #try function 
  try(my_sd <- sd(my_germ[,i], na.rm = TRUE)) #try function 
  try(print(c(my_mu, my_sd)))
}
###Results: errors since this has NA as part of the columns 


#Create the same results by using a UDF for one variable as an imput to calculate the mean and standard deviation
musigma <- function(var1){#also call function operators
  try(my_mu <- mean(var1, na.rm = TRUE))
  try(my_sd <- sd(var1, na.rm = TRUE))
  try(return(print(c(my_mu, my_sd))))
}
  #Best practice
musigma(my_germ$duration)  


my_uniform <- sample(c(1,2,3,4,5,6), size=100, replace=TRUE)
mean(my_uniform)
hist(my_uniform)

my_binary <- sample (c(0,1), size=1000000, replace=TRUE) 
mean(my_binary)                    
hist(my_binary)


my_exp <- rexp(100000, rate=2)
mean(my_exp)
hist(my_exp)

#Creating a for loop to check histograms for all my germ
for(i in 1:ncol(my_germ)){
  try(hist(my_germ[,i]))
} #closing the i loop

##### Data Science: CLASS 06 - Creating visuals#######################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 06 - Creating visuals 
##### Date: 22.10.2021
##### Version 0.1


##### User defined functions --> UDFs and desc statistics 
#Creating visuals 
library(ggplot2)

#Depending on the marital status and the amount
ggplot(data=my_germ, aes(x=marital, y=amount, color=good_bad))+
  geom_jitter()
#Conclusion: the marital status 3 = married with kids is the worst

#Do kids impact on the amount
ggplot(data=my_germ, aes(x=depends, y=amount, color=good_bad))+ 
  geom_jitter()
#Conclusion: whit more dependents we ask for less money

#Duration and the amount
my_scatter <- ggplot(data=my_germ, aes(x=duration, y=amount, color=good_bad))+ 
              geom_point()+
              geom_smooth()+
              scale_color_manual(values = c("#B81CEE","#0AF0D4"))
              #https://htmlcolorcodes.com/ to look for colors in html #+code

#Plotly library to make dinamic graph
#install.packages("plotly")
library(plotly)

ggplotly(my_scatter)


##### Data Science: CLASS 06 - Standardization ###########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 06 - Standardization
##### Date: 22.10.2021
##### Version 0.1


#Standardization of a normal distribution

standard <- function(var1){
  my_standard <- (var1-mean(var1))/sd(var1)
  return(my_standard)
} #Closing the standard variable

my_germ$age_standard <- standard(var1 = my_germ$age)
my_germ$amount_standard <- standard(var1 = my_germ$amount)

mean(my_germ$age_standard)
mean(my_germ$amount_standard)

sd(my_germ$age_standard)
sd(my_germ$amount_standard)


#T-score standardization of a normal distribution

standard <- function(var1){
  my_standard <- ((var1-mean(var1))/sd(var1))*10+50
  return(my_standard)
} #Closing the standard variable

my_germ$age_standard <- standard(var1 = my_germ$age)
my_germ$amount_standard <- standard(var1 = my_germ$amount)

mean(my_germ$age_standard)
mean(my_germ$amount_standard)

sd(my_germ$age_standard)
sd(my_germ$amount_standard)

summary(my_germ$age_standard)

#Normalizing our data

normal <- function(var1){
  my_normal <- (var1 - min(var1))/(max(var1) - min(var1))
  return(my_normal)
}#closing the normal UDF

my_germ$checking_norm <- normal(var1= my_germ$checking)
my_germ$duration_norm <- normal(var1= my_germ$duration)
my_germ$amount_norm <- normal(var1= my_germ$amount)
my_germ$employed_norm <- normal(var1= my_germ$employed)
my_germ$installp_norm <- normal(var1= my_germ$installp)
my_germ$age_norm <- normal(var1= my_germ$age)
my_germ$existcr_norm <- normal(var1= my_germ$existcr)
my_germ$telephon_norm <- normal(var1= my_germ$telephon)

#Creating training and testing data sets 
train_index <- sample(1:nrow(my_germ), size = 0.8*nrow(my_germ)) #sample the index of observations

germ_train <- my_germ[train_index,] #we use the train_index because we want the 800 customers 
germ_test <- my_germ[-train_index,]#excluding the 800 to get the 200 observation acording to the thumb rule 80-20%

#stratified code the good_bad variable


##### Data Science: CLASS 07 - Regressions ###########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 07 - Regressions 
##### Date: 22.10.2021
##### Version 0.1


# Creating a predictive model - linear regression

my_linear <- lm(amount~age, data=germ_train)
summary(my_linear)

# Creating logistic regression model for data with units
# Multivariate Regression

my_logit <- glm(binary~checking+duration+age+telephon+amount+savings+installp+coapp, 
                  data=germ_train, family="binomial")
summary(my_logit)

# Removing our statistically insignificant vraiable, "telephon" to better our model
my_logit_better <- glm(binary~checking+duration+age+amount+savings+installp+coapp, 
                         data=germ_train, family="binomial")
summary(my_logit_better)       

    exp(0.5721)-1
    #0.7719 For every additional unit on checking, the binary successful odds will increase in 77.19% 

# Designing a unitless logistic regression

my_logit_norm <- glm(binary~checking_norm+duration_norm+age_norm+
                         amount_norm+installp_norm, data=germ_train, family="binomial")
summary(my_logit_norm)
  #The variable with most impact is checking, while the most negative one is duration

##### Data Science: CLASS 08 - Preformance evaluation ###########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 08 - Preformance evaluation
##### Date: 03.11.2021
##### Version 0.1

#install.packages("caret")
library(caret)

#Testing data
#Creating a confusion matrix
my_prediction_testing <- predict(my_logit, germ_test, type="response")

#Applying the function for testing data confusionMatrix(prediction, and the labels of our data)
confusionMatrix(data= as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference = as.factor(as.numeric(germ_test$binary)))
  #It is a good model...

#Training data
#Creating a confusion matrix
my_prediction_training <- predict(my_logit, germ_train, type="response")

#Applying the function for training data confusionMatrix(prediction, and the labels of our data)
confusionMatrix(data= as.factor(as.numeric(my_prediction_training > 0.5)),
                reference = as.factor(as.numeric(germ_train$binary)))


#Creating an  AUC ROC framework
#install.packages("ROCR")
library(ROCR)  


my_prediction <- my_prediction_training

#converting the data to use the ROCR
pred_val_logit <- prediction(my_prediction, germ_train$binary)

#simulate the matrices 
pref_logit <- performance(pred_val_logit, "tpr", "fpr")

#Visualization of the plot
plot(pref_logit)
#This is boring for one model, but more interesting for various models 

#Creating the challenging model
#Creating the GINI index
library(rpart)
library(rpart.plot)

#replace glm with rpart and family with method = "class"
my_tree <- rpart(binary~checking+duration+age+telephon+amount+savings+installp+coapp, 
                data=germ_train, method = "class", 
                cp = 0.02)

#Ploting the tree
rpart.plot(my_tree, extra = 1, type = 1)

##### Data Science: CLASS 09 - Regressions###########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 09 - Regressions 
##### Date: 04.11.2021
##### Version 0.1

#Comparing model performance
my_tree_predict_test <- predict(my_tree, germ_test, type="prob")
my_tree_predict_train <- predict(my_tree, germ_train, type="prob")

#Converting to ROCR but we need to consider that it has two variables [,2]
my_tree_prediction <- prediction(my_tree_predict_train[,2], germ_train$binary)
my_tree_performance <- performance(my_tree_prediction, "tpr", "fpr")

#Visualization of the plot
plot(my_tree_performance, col="black")
#Ploting the previous preformance with a red color into the same curve with add=TRUE
plot(pref_logit, col="red", add=TRUE)
#We are for the predictions that are better for small numbers, therefore, we prefer the red curve. 
#Both lines represent how much good is the information presented 

#Another way to determine which model is better 

#Add performance, compare, tree,  them to @Tag professor on linkedin with this document 

##### Data Science: CLASS 10 - Shinny Web app ###########################################################################################################
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 10 - Shinny Web app 
##### Date: 04.11.2021
##### Version 0.1

hist(rexp(1000, rate=.4), breaks = seq(from=0, to=30, by=1)) 
hist(rexp(1000, rate=1), breaks = seq(from=0, to=30, by=1))
hist(rexp(1000, rate=2), breaks = seq(from=0, to=30, by=1))
rexp(1000, rate=2)

##### Text Analytics: CLASS 01 - Piping #######################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 01 - Piping
##### Date: 16.11.2021
##### Version 0.1
###This is how multiple objects looks like.

#install.packages("magrittr")
#install.packages("dplyr")

library(magrittr)
library(dplyr)

#Multiple objects(learn in previous modules) easier to read but not efficient on the memory
print(mtcars)
a <- filter(mtcars, carb>2)
b <- group_by(a, cyl)
c <- summarise(b, Avg_mpg = mean(mpg))
d <- filter(c, Avg_mpg > 15)
print(d)

#Nested objects: Memory efficient but hard to read
z <- filter(
  summarise(
    group_by(
      filter(
        mtcars, carb > 2),#Closing filter
      cyl
    ), #Closing group_by 
    Avg_mpg = mean(mpg)
  ), #Closing summarise
  Avg_mpg > 15
) #Closing filter

mtcars
#PIPING METHOD: easy to read and memory efficient CTRL + shift + M on Mac
piped_df <- mtcars %>% #Call data
  filter(carb>2)%>% #use the data and filter
  group_by(cyl)%>% #use the filter data and group it
  summarise(avg_mpg = mean(mpg))%>%   #use the filter, group data and summarise
  filter(avg_mpg > 15) #use previous data to filter

#Disadvantage: 
#Is hard to find mistakes because we can not see where in the code is the mistake.
#We need a library
#Have to be very carefull on the step by step 

### IN CLASS 01
library(magrittr)
library(dplyr)

example_1 <- mtcars %>% #Call data
  filter(carb > 1) %>% #use the data and filter
  group_by(cyl) %>% #use the filter data and group it
  summarise(Avg_mpg = mean(mpg)) %>%   #use the filter, group data and summarise
  arrange(desc(Avg_mpg)) #use previous data to order them

my_germ

#PIPING ON MY GERM DATA BASE
#Filtering property > 3, and only selecting checking and saving columns from my_germ database
library(dplyr)
my_germ_piped <- my_germ %>%
                    filter(property > 3) %>%
                    select(checking, savings)

#How many observations are good and how many are bad
my_germ_piped2 <- my_germ %>%
                  group_by(good_bad) %>%
                  summarise(my_count = n()) #Smaller case n because is for the sample not the population (N)

#summarize function, calculate the sum of binary, and the mean of housing for every level of job
library(dplyr)
my_germ_piped3 <- my_germ %>%
                  group_by(job) %>%
                  summarise(sum_binary = sum(binary), avg_housing = mean(housing))


#Custom risk score: 0.3amount+0.4history+0.5*age and filter this score for values higher than 100
library(dplyr)
my_germ_piped4 <- my_germ %>%
                  mutate(my_score = 0.3*amount+0.4*history+0.5*age) %>% #mutate= creates a new variable to our data set
                  filter(my_score > 100)

##### Text Analytics: CLASS 02 - Tokenization and frequencies ######################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 02 - Tokenization and frequencies
##### Date: 18.11.2021
##### Version 0.1

#install.packages("tidytext")
#install.packages("tidyverse")
#install.packages("stringr")

library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

#01. Creating a vector with long sentences
my_txt <- c("I think that data is the new bacon", 
            "What is more. I think text analytics is the new turkey bacon", 
            "very few people know how good Canadian bacon is",
            "even fewer people know how a beaver tails tastes like",
            "Putin french fries are not so good")

#02. Putting the vector in a data frame
mydf <- data.frame(line=1:5, text=my_txt)
print(mydf) #5 observation and 2 variables


        #In case we want to upload a file: To be confirm 
        #Mydf <- data.frame(xyz.csv)
        #Mydf<-data.frame(path.csv)

#03. Tokenizing the mydf dataframe
token_list <- mydf %>%
              unnest_tokens(word,text) #takes the text and creates a vector at word level (only one word), takes out the punctuation. To maintain punctuation include it, unnest_tokens(word, text, token = "words / characters / sentences", stript_punct = F)
print(token_list)

#04. Token frequencies
frequency_tokens <- mydf %>%
                    unnest_tokens(word,text) %>% #Creates the single words
                    count(word, sort =TRUE) %>%  #Counts how many time the word appears
                    drop_na() #helps to drop na if existed

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
                count(word, sort =TRUE) %>% #Counting words frequency base on "n" number of times the word appears
                mutate(word=reorder(word, n)) %>% #Updates the word variable in a descending order
                #Plotting the counting words graph
                ggplot(aes(word, n, fill=word))+ #words in x and n count in y, the fill will give the color if wanted by the choosen variable
                geom_col()+ #creates bar charts, if we want histograms we should use geom_histogram(binwidth=XX)
                labs(x = "words in text of the reading", y = "number of repetition")+ #Creating the title axes for x and y
                ggtitle("Words count by word frequency") + #Creating the title of the graph
                theme_bw()+ #choosing the theme for the graph
                coord_flip() #flipping from vertical to horizontal
  
  print(freq_hist)

  
#IN CLASS 02

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
setwd("/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/txt")

nm <- list.files(path="/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/txt")
#using read document to import the data:
my_data <- read_document(file=nm[1]) #This comes out as a vector
my_data_together <- paste(my_data, collapse = " ") # This will give us a concatenated vector

my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))

#Importing all .doc files from one directory
#install.packages("textshape") #for some reason textreadr has issues getting textshape
#install.packages("textreadr")

library(textreadr)
setwd("/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/doc")
nm <- list.files(path="/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/doc")
my_doc_text <- do.call(rbind, lapply(nm, function(x) read_doc(file=x)))


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



##### Text Analytics: CLASS 03: VCorpus ---------
################## Corpus object

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
setwd("/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/pdf/")
nm <- list.files(path="/Users/lizherrera/Library/Mobile Documents/com~apple~CloudDocs/Documents/Hult/MsBA/05. NLP/pdf/")

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

opinions # I want to see the VCoprus content
opinions[[1]] # I want the first doc, this is a list so [[]] needs to be applied
#let's take a look at the Corpus that was created 
#if you want to get some metadata from the i-th object:
opinions[[1]]$meta$author

#Exercises:
#Try to find the author's name for the 7th document
#try to find the document ID in for the 4th document


##### Text Analytics: CLASS 03 - DTM format + Netflix case#####################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 03 - DTM format + Netflix case
##### Date: 20.11.2021
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

##### Text Analytics: CLASS 03: Correlograms, Correlations and DTMS---------------
#09 let's plot the correlograms
library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, #United States is the benchmark, we will compare the other countries to the other ones
                      color = abs(`United States`- proportion)))+
  geom_abline(color="red4", lty=)+ #adding the division for the correlation, colors() provides the pallet of colors available. lyt: references to the linetype 0: blank, 1: solid, 2: dashed, 3: Doted, 4: dotdash, 5; longdash, 6: twodash
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+ #alpha gives the transparency, jitter gives the scatter plot with a bigger distribution than geom_point
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) + #adding the labels of the data
  scale_x_log10(labels = percent_format())+ #Transforming the x axis to a logarithmic scale of 10
  scale_y_log10(labels= percent_format())+ #Transforming the y axis to a logarithmic scale of 10
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+ #adding the gradiant for the data, it needs a low and high value. If we want a mid value we should use scale_color_gradient2(Low="xx", high="xx", mid ="xx")
  facet_wrap(~author, ncol=2)+ #Creating a breakdown of charts by author. If we want each one to have there own scale: face_wrap(~variable, scales ="free y"), and if we want it to start at zero: expand_limits(y=0)
  theme(legend.position = "none")+ #creating a theme for the graph
  ggtitle("Words distribution per author (Country)") + #Creating the title of the graph
  labs(y= "United States", x=NULL) #adding the y label, x label is nul becaus it will depend on the country

#Business insights: 
#The words next to the line appear in both countries 
#The words that are away from the diagonal are only seen in that country
#

#09 let's plot the correlograms
library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, #United States is the benchmark, we will compare the other countries to the other ones
                      color = abs(`United States`- proportion)))+
  geom_smooth(formula = y ~ x, method = "lm", se = T, level = 0.95) #Creating the regression line: geom_smooth(method: (NULL/lm/glm/gam/loess), formula: (y ~ x / y ~ poly(x, 2) / y ~ log(x) / NULL), se: (T/F) interval confidence, level: 0.95 confidence level)
  
  help("geom_smooth")


#10 Doing the cor.test()
#Test for association between paired samples, using one of Pearson's product moment correlation coefficient

cor.test(data=frequency[frequency$author == "Brazil",], #Comparing data from USA to Brazil
         ~proportion + `United States`) #cor 0.6594891 Moderate

cor.test(data=frequency[frequency$author == "India",], #Comparing data from USA to India
         ~proportion + `United States`) #cor 0.6593853 Moderate

#11 DTM

netflix_dtm <- netflix %>% #Creating a dtm
  unnest_tokens(word, text) %>% #tokenization 
  count(title, word) %>% #counting the times the word appears
  cast_dtm(title, word, n) #This turns a "tidy" one-term-per-document-per-row data frame into a DocumentTermMatrix 
netflix_dtm #sparsity 100%, the closest to 100% is the better

##### Text Analytics: CLASS 04 - Sentiment Analysis ######################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 04 - Sentiment
##### Date: 23.11.2021
##### Version 0.1


#01 Creating the DTM format from previous section
library(tidytext)
library(tidyverse)
library(dplyr)
library(tidyr)

netflix %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(country, word) %>% #until here without the pipping sign, it is the tidy format
  cast_dtm(country, word, n) #makes all numeric, this is the dtm format
  #<<DocumentTermMatrix (documents: 682 countries, terms: 18432 tokens)>>
  #Sparsity: 100%


#Pre class videos


# Sentiment in tidytext()
#install.packages("textdata")
library(textdata)
library(tidytext)
afinn <- get_sentiments("afinn")#from -5 to +5 and 0 means neutral
nrc <- get_sentiments("nrc") #gives the flavors
bing <- get_sentiments("bing") #positive or negative

library(dplyr)
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"), #combining all the data source into one 
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)


sentiments %>%
  filter(lexicon == "bing") #binary, while nrc has the flavors

unique(sentiments$sentiment) #this is the qualitative sentiment that we can get
summary(sentiments$value) # this is 3rd score that we can use / quantitative


##### Lets take a look at the lexicons one by one

#how can we subset the data to get distinct lexicons?

bing_sentiment <- subset(sentiments, lexicon == "bing")
filter(bing_sentiment, sentiment == "negative") # these are the nrc options of sentiment labels

## This is a code starter for your take home exercise
## If you want to practice the code on other lexicons
#1 : Take a look at the sentiment for bing
bing_data <- subset(sentiments, lexicon == "bing")
unique(bing_data$sentiment)

#2 Take a look at the sentiment value for afinn
afinn_data <- subset(sentiments, lexicon == "afinn")
summary(afinn_data$value)

###IN CLASS 
### Sentiment analysis with Netflix

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_token <- netflix %>%
  unnest_tokens(word, text) #Tokenize data

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise") #filter by surprise

#inner joining the India movies and the surprise sentiments
netflix_token %>% #This is a frequency table on tidyformat
  filter(country == "India") %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

##### Comparing different sentiment libraries on Netflix

india <- netflix_token %>%
  filter(country == "India")

afinn <- india %>%
  inner_join(get_sentiments("afinn"))%>% #Bringing the afinn library
  summarise(sentiment=sum(value)) %>% #summarise the data
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  india%>%
    inner_join(get_sentiments("bing"))%>% #combine the tokenize india with Bing
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>% #how they interact with eachother in a graph
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

######## Most common positive and negative words

bing_counts <- india %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() #to analyze data

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  ggtitle("Sentiment frequency on bing library") + #Creating the title of the graph
  theme_bw()+
  coord_flip()




##### Text Analytics: CLASS 04 - N-grams and tokenizing #######################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 04 - N-grams and tokenizing
##### Date: 30.11.2021
##### Version 0.1


#install.packages("janeaustenr")
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)



#unnest_tokens(word, text)
#unnest_tokens(bigram, text, token = "ngrams", n=2)
#unnest_tokens(quadrogram, text, token = "ngrams", n=4)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>% 
  drop_na()
#tokenizing by pairs = bigrams (input: bigram, text, token="": ngrams, n=#:2)
#the n is what mathers the most

austen_bigrams 
#We want to see the bigrams (words that appear together, "pairs")

austen_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 
#What bigrams are the most frequent? stop words

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
#we are separating the bigrams into "word1" and "word2" separated by a space " "
#This would give us two separated tokens 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #%in% this would help us to filter out word1 in the stop word and removing them 
  filter(!word2 %in% stop_words$word) #%in% this would help us to filter out word2 in the stop word and removing them

#creating the new bigram, "no-stop-words":
bigram_counts  <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

#want to see the new bigrams
bigram_counts


####### VISUALISING A BIGRAM NETWORK

#install.packages("igraph") --> graphs and network analysis
#packageDescription("igraph")

library(igraph) 
bigram_graph <- bigram_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

bigram_graph

#Creating the arrow open will be "➔" while close will be like "➡︎" 
a <- grid::arrow(type="open", length = unit(.10,"inches")) 

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") + #"fr" means frequency
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a)+ #including the arrow direction to the graph
  geom_node_point(color="#009999", size = 2)+ #changing color and size of the node http://www.sthda.com/english/wiki/colors-in-r  
  geom_node_text(aes(label=name), vjust =1, hjust=1) + #label or word in this case
  theme_gray() #choosing the theme for the graph


###### What if we are interested in the most common
################ 4 consecutive words - quadro-gram

quadrogram <- austen_books() %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>% #tokenizing
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>% #separating quadrogramas by ""
  drop_na() %>% #dropping nas
  filter(!word1 %in% stop_words$word) %>% #filtering stop words in word1
  filter(!word2 %in% stop_words$word) %>% #filtering stop words in word2
  filter(!word3 %in% stop_words$word) %>% #filtering stop words in word3
  filter(!word4 %in% stop_words$word)     #filtering stop words in word4

quadrogram

#Counting and filtering quadrograms

quadrogram_graph  <- quadrogram %>%
  count(word1, word2, word3, word4,  sort = TRUE) %>% #counting quadrogrmas 
  filter(n>1) %>% #filtering 
  graph_from_data_frame()

quadrogram_graph

#Creating the arrow open will be "➔" while closed will be like "➡︎" 
b <- grid::arrow(type="closed", length = unit(.08,"inches")) 

ggraph(quadrogram_graph, layout = "fr") + #"fr" means frequency
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = b)+ #including the arrow direction to the graph
  geom_node_point(color="#FF99FF", size = 2)+ #changing color and size of the node http://www.sthda.com/english/wiki/colors-in-r  
  geom_node_text(aes(label=name), vjust =3, hjust=3) + 
  theme_light() #choosing the theme for the graph

##### Text Analytics: CLASS 05 - Sentiment analysis with Netflix#######################################################################################################-------------------------
##### Class: Text Analytics and Natural Language Processing 
##### Created by Liz Herrera
##### MBAN2 HULT 2021
##### CLASS 05 - Sentiment analysis with Netflix
##### Date: 30.11.2021
##### Version 0.1


library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_token <- netflix %>%
  unnest_tokens(word, text)

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

#inner joining the India movies and the surprise sentiments
netflix_token %>% #This is a frequency table on tidyformat
  filter(country == "India") %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)


##### Comparing different sentiment libraries on Netflix 


india <- netflix_token %>%
  filter(country == "India")

afinn <- india %>%
  inner_join(get_sentiments("afinn"))%>% #Bringing the afinn library
  summarise(sentiment=sum(value)) %>% #summarise the data
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  india%>%
    inner_join(get_sentiments("bing"))%>% #combine the tokenize india with Bing
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>% #how they interact with eachother in a graph
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


######## Most common positive and negative words


bing_counts <- india %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


###### N-grams and tokenizing


library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

netflix_bigrams <- netflix %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

netflix_bigrams #We want to see the bigrams (words that appear together, "pairs")

netflix_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- netflix_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


###### What if we are interested in the most common
################ 4 consecutive words - quadro-gram

quadrogram <- netflix %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram


###### We can also apply the tf_idf framework
########### on our bigram and quadro-gram


bigram_united <- xxxxxx %>%
  xxxxxx(xxxx, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- xxxxxx %>%
  count(country, bigram) %>%
  xxxxxxx(bigram, xxxxxx, n) %>%
  xxxxxx(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(country, quadrogram) %>%
  bind_tf_idf(quadrogram, country, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf


######## visualising negated words
###### negated words in sentiment analysis


negation_tokens <- c("no", "never", "without", "not")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, score, sort=TRUE) %>%
  ungroup()

negated_words


#### we can visuals the negated words
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*score, fill = n*score >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not")
negated_words_plot(x="no")
negated_words_plot(x="without")


####### VISUALISING A BIGRAM NETWORK

#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



###Pairwise correlations between words


#install.packages("widyr")
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

my_tidy_df <- netflix %>%
  xxxxx(country == "xxxxxxx") %>%
  xxxxxx(word, text) %>%
  filter(!word %in% xxxxxx)

my_tidy_df
#taking out the least common words
word_cors <- my_tidy_df %>%
  group_by(xxxx) %>%
  filter(n() >= 5) %>%
  xxxxxx(word, title, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "love")

####### creating barcharts for correlatoins

word_cors %>%
  filter(item1 %in% c("love", "students", "killer", "death")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()


####### creating a correlation network


#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >.4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=5)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()




###### Latent Dirichlet algorithm


# There are two principles:
#1. Every document is a combination of multiple topics
#2. Every topic is a combination of multiple words or tokens
#If those two principle do not apply, do not use the model 


#install.packages("topicmodels")
library(topicmodels) #new articles from American news agencies
data("AssociatedPress") #This is a DTM format 

AssociatedPress

#calling the Latent Dirichlet Allocation algorithm
ap_lda <- LDA(AssociatedPress, k=2, control=list(seed=123)) #K=2 means only two topics
ap_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics
library(ggplot2)
library(dplyr)
library(tidyr)

top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend =FALSE) +
  facet_wrap(~topic, scale = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>0.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1))

beta_spread


###### LDA per chapter based on Gutenber


library(gutenbergr)
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#devide into documents, each representing on e chapter
library(xxxxxxx)
reg <-xxxxxxxx(xxxxxxxxx, ignore_case=TRUE)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter=cumsum(str_detect(text,reg))) %>%
  ungroup() %>%
  filter(chapter>0) %>%
  unite(document, title, chapter)

#split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

#find document-word counts
word_counts <- xxxxxxxxx %>%
  xxxxxxxxx(stop_words) %>%
  count(xxxxxxxx, xxxxxx, sort = TRUE) %>%
  ungroup()

print(word_counts)

chapters_dtm <- word_counts %>%
  xxxxxxxxxx(xxxxxxxx, word, n)

chapters_lda <- xxxxxxx(chapters_dtm,k=xxxxxx, control = list(seed=123))
chapters_lda


#### Per documnet classification


chapters_gamma <- tidy(xxxxxxxx, matrix=xxxxxxx)
chapters_gamma 

chapters_gamma <- xxxxxxxx %>%
  xxxxxxxxx(document, c("title", "chapter"),sep = "_", convert=TRUE)

chapters_gamma

chapters_gamma %>%
  xxxxx(title=reorder(title, xxxxxxx*xxxxxxxx)) %>%
  ggplot(aes(factor(topic), gamma)) +
  xxxxxxxxxxxx()+
  facet_wrap(~title)

chapter_classifications <- xxxxxxxxxxxx %>%
  group_by(xxxxxxxxxxxx, xxxxxxxxxxx) %>%
  top_n(1, xxxxxxxxx) %>%
  xxxxxxxxxx()

chapter_classifications



##### Text Analytics: CLASS 05 - TF-IDF framework in Netflix -------

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

##### Text Analytics: CLASS 05 - Pairwise correlations between words----- 

#install.packages("widyr")
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles
colnames(netflix)[12] <- "text"

my_tidy_df <- netflix %>%
  filter(country == "United States") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

my_tidy_df
#taking out the least common words
word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, title, sort=TRUE)
#pairwise_cor() check correlation based on how ofter words appear in the same section

word_cors %>%
  filter(item1 == "love")

####### creating barcharts for correlatoins


word_cors %>%
  filter(item1 %in% c("love", "students", "killer", "death")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

####### creating a correlation network


#this will take some time to run, we will need to wait for the result
# feel free to adjust the geom_node_point to somehting smaller

word_cors %>%
  filter(correlation >.3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

##### Text Analytics: CLASS 06 - Machine Learning - LDA Beta-----

# There are two principles:
#1. Every document is a combination of multiple topics
#2. Every topic is a combination of multiple words

install.packages("topicmodels")
library(topicmodels) #new articles from American news agencies
data("AssociatedPress")

AssociatedPress

#calling the Latent Dirichlet Allocation algorithm
ap_lda <- LDA(AssociatedPress, k=2, control=list(seed=123)) #"k" = number of topics you want your LDA model to have...
ap_lda

#now we are looking for the per topic per word probabilities aka. beta BETA MATRIX
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics
library(ggplot2)
library(dplyr)
library(tidyr)

top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + #term = "x" variable and beta = "y variable"
  geom_col(show.legend = FALSE) + #Creating Bar Charts
  facet_wrap(~topic, scales = "free") + #Creating divisions depending on variables after the "~" sign
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- ap_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic1/topic2))

beta_spread #The more positive the log_rate is = the strongest word related to Topic 1, while the more negative the log_rate is = the strongest word related to Topic 2#

##### Text Analytics: CLASS 06 - Machine Learning - LDA Gamma-----
### After class practice: Latent Dirichlet algorithm

library("tidytext")
library("tidyverse")
library("tidyr")
library("tidytuesdayR")
library("stringr")
library("textreadr")
library("pdftools")
library("textshape")
library("twitteR")
library("tm")
library("ggplot2")
library("scales")
library("magrittr")
library("dplyr")
library("gutenbergr")
library("Matrix")
library("textdata")
library("igraph")
library("ggraph")
library("widyr")
library("topicmodels")
library("quanteda")
library("quanteda.textmodels")
library("RColorBrewer")


###### LDA per chapter based on Gutenber

chapters_gamma <- tidy(ap_lda, matrix="gamma") #allows us to predict a topic for a given document. Predicting the probability of a topic for a given document is based on, what we call, the "gamma" matrix.
chapters_gamma #this will show the results in the console

#Retreiving data from the Gutemberg project
library(gutenbergr)
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

#devide into documents, each representing one chapter
library(stringr)
reg <-regex("^chapter ", ignore_case=TRUE)
by_chapter <- books %>% #dividing the books by chapter
  group_by(title) %>% #grouping by title
  mutate(chapter=cumsum(str_detect(text,reg))) %>% #creating the chapter as a new column
  ungroup() %>% #ungrouping for a text analysis
  filter(chapter>0) %>%
  unite(document, title, chapter)

#split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text) #tokenizing

#find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>% #removing stop words
  count(document, word, sort = TRUE) %>% #counting frequency
  ungroup() #ungrouping for the LDA analysis

print(word_counts)

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n) #creating the dtm format 

chapters_lda <- LDA(chapters_dtm,k=4, control = list(seed=1234)) #creating the LDA ranking, K means the number of topics we want to predict
chapters_lda

#### Per documnet classification

#Putting the chapters back together in the correct book. 
#Beta means per-topic-per-word probability
#Gamma means per-document-per-topic probability
chapters_gamma <- tidy(chapters_lda, matrix="gamma")
chapters_gamma 

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"),sep = "_", convert=TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title=reorder(title, gamma*topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()+
  facet_wrap(~title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications


