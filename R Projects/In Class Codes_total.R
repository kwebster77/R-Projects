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
##### CLASS 10 - Shinny Web app 
##### Date: 04.11.2021
##### Version 0.1

hist(rexp(1000, rate=.4), breaks = seq(from=0, to=30, by=1)) 
hist(rexp(1000, rate=1), breaks = seq(from=0, to=30, by=1))
hist(rexp(1000, rate=2), breaks = seq(from=0, to=30, by=1))
rexp(1000, rate=2)
