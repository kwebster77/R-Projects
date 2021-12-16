##########################################
############ Created by Karley Webster
######### MBAN2 Hult 2021
############## Date: 10.14.2021
######### Version 0.1
##########################################

# Reading an excel file from the local drive 
library(readxl)
my_germ <- read_excel("Desktop/german credit card.xls")
View(my_germ)

# Creating a few random objects 

# Scalars 
count_questions <- 21
count_customers <- 1000

# creating a vector 
germ_dim <- c(count_customers, count_questions)

#new type of vector
customer_age <- c(16, 23, 29, 25, 26, 23, 25, 24, 25, 45)
age_matrix <- matrix(customer_age, nrow = 2 , ncol = 5, byrow = TRUE)
View(age_matrix)

#subsetting
customer_age[6:10]
my_germ[1:100 , c("age", "amount")]
View(my_germ[1:100 , c("age", "amount")])

#creating a list
my_list <- list(my_germ, age_matrix, count_customers, count_questions)
View(my_list)

# testing and data types 
is.numeric(my_germ$good_bad)
is.character(my_germ$good_bad)

as.numeric(my_germ$good_bad)
as.numeric(my_germ$purpose)

#using the which () to subset and filter based on business conditions 
#this helps to locate those with a specific data- to filter -- duration of these were interesting to note
which(my_germ$purpose == "X")
my_germ[which(my_germ$purpose == "X") ,]

#analyzing good and bad customers- trying to identify which customers are good/bad and analyzing this data 
summary(my_germ[which(my_germ$good_bad == "good") , ])

summary(my_germ[which(my_germ$good_bad=="bad") , ])

#fixing the data with gsub()

as.numeric(gsub("X" , "" , my_germ$purpose))

#creating a new variable which is saved 
my_germ$purpose_fixed <- as.numeric(gsub("X" , "" , my_germ$purpose))

# running gsub to change the good and bad to 1 and 0, respectively 
my_germ$binary <- gsub("good", "1" , my_germ$good_bad)
my_germ$binary <- gsub("bad", "0" , my_germ$binary)

# changing this from character to number (because this was a character from good to 1), 
#then we 'resaved' it over to the binary 
my_germ$binary <-as.numeric(my_germ$binary)


#Session 4 - for loops and if statements-- use [,i] because the variables are in the columns 
#good coding practice to redefine the class of an object
my_germ <- as.data.frame(my_germ)

#the na.rm helps to remove all the empty spots from the data
for(i in 1:3){
  print(min(my_germ[,i], na.rm = TRUE))
  print(max(my_germ[,i], na.rm = TRUE))
  print(mean(my_germ[,i], na.rm = TRUE))
  
}#closing the i loop 

#saving the risk score in a new variable- this can be an empty vector 
my_germ$score <- c()

#create a new for loop to look at multiple variables with weights 
#since only using one variable, it becomes a vector which only has one index (why we use the [i])
for(i in 1:nrow(my_germ)){
my_germ$score [i] <-  
  0.5*my_germ$duration[i] +
    0.1*my_germ$amount[i] +
      0.1*my_germ$age[i] +
        0.3*my_germ$installp[i]
  
  
}#closing i loop 

## practice for loop for the team assignment
my_germ$practice_score <- c()

for(i in 1:nrow(my_germ)){
  my_germ$practice_score [i] <-  
    0.2*my_germ$savings[i] +
    0.3*my_germ$job[i] +
    0.3*my_germ$duration[i] +
    0.2*my_germ$history[i]
  
}#closing i loop 

#creating a for loop to label the customers as 'outstanding' based on custom logic (lower than 500 score)
#use empty c function to being 
my_germ$label <-c()

for(i in 1:nrow(my_germ)){
  if (my_germ$score[i] < 500 & my_germ$binary[i] ==1) {
    my_germ$label[i] <- "outstanding"
  }else{
    my_germ$label[i] <- "not outstanding"
  }#closing my if statement <500 and binary 
  
}#closing the i loop


#UDFs and descriptive stats 

team_score <- function(var1, var2, var3, var4, w1, w2, w3, w4){
  #use this to put in what you are going to put together for the my_score
  my_score <- w1*var1 + w2*var2 + w3*var3 + w4*var4
  min_score <- min(my_score, na.rm= TRUE)
  max_score <- max(my_score, na.rm= TRUE)
  mean_score <- mean(my_score, na.rm= TRUE)
  return(c(min_score, max_score, mean_score))
}
#these are creating our team scores based on our previous variables and weights, two team practices 
team_10 <- team_score(var1 = my_germ$duration, w1=0.4,
           var2 = my_germ$amount, w2=0.2,
           var3 = my_germ$history, w3=0.2,
           var4 = my_germ$job, w4=0.2)
           
team_4 <- team_score(var1 = my_germ$duration, w1=0.3,
                      var2 = my_germ$amount, w2=0.2,
                      var3 = my_germ$history, w3=0.2,
                      var4 = my_germ$employed, w4=0.3)
                
#re-coding using best coding practice 
team4_best <- team_score(my_germ$duration, my_germ$amount, my_germ$history, my_germ$employed,
                         0.3, 0.2, 0.2, 0.3)
#look into the source function 
?source

#calculating mu and sigma for all the variables (columns)

for(i in 1:ncol(my_germ)){
  try(my_mu <- mean(my_germ[,i], na.rm =TRUE)) 
  try(my_sd <- sd(my_germ[,i], na.rm =TRUE))
  try(print(c(my_mu, my_sd)))
  
}
#returns a large variation/ranges between the variables- because there are different unit 
#this creates a challenge for predictive modelling 
#hard to compare the impact on our business success

#create a UDF for a specific variable (same result for one variable)


####test####
#UDF_score <- function(){
  #use this to put in what you are going to put together for the my_score
  #UDF_score <- var1
  #std_score <- std(my_score, na.rm= TRUE)
  #mean_score <- mean(my_score, na.rm= TRUE)
  #return(c(std_score, mean_score))
#}
##### test ####
mu_sigma_fx <- function(x) {
  try(my_mu <- mean(x, na.rm = TRUE)) 
  try(my_sd <- sd(x, na.rm = TRUE)) 
  try(return(c(my_mu, my_sd)))
}

#creating a uniform distribution 
#replace = TRUE because you need to replace the vectors (ie- it's 100 but only numbers 1-6)
my_uniform <- sample(c(1,2,3,4,5,6), size=10, replace=TRUE)
mean(my_uniform)
hist(my_uniform)

#binary scores 
my_binary <- sample(c(0,1), size = 5, replace =TRUE)
mean(my_binary)
hist(my_binary)

#exponential distribution using lambda 
my_exp <- rexp(100000, rate = 0.5)
mean(my_exp)
hist(my_exp)

#creating a for loop to check histograms for all my_germ variables 
for (i in 1:ncol(my_germ)){
  try(hist(my_germ[,i]))
  
}#closing the i loop 


#standardization- making a UDF for creating a z-score 
standard <- function (var1){
  my_standard <- (var1-mean(var1))/sd(var1)
  return(my_standard)
} #closing the standard variable 

#converting standard amount to a new variable using our function above 
my_germ$age_standard <- standard(var1=my_germ$age)
#converting standard amount to a new variable using our function above 
my_germ$amount_standard <- standard(var1=my_germ$amount)
#checking the mean for our new standards 
mean(my_germ$age_standard)
mean(my_germ$amount_standard)
#checking the standard deviation of our new standards 
sd(my_germ$age_standard)
sd(my_germ$amount_standard)

#Z-score has negative values for age when using the z-score 
range(my_germ$age_standard)

#there is a way to better standardize the z-score: use the t-score instead 
#convert our standard function to a t=score 

standard_tscore <- function (var1){
  my_standard <- ((var1-mean(var1))/sd(var1))*10 +50
  return(my_standard)
} #closing the standard variable 

#converting to t score
my_germ$age_tstand <- standard_tscore(var1=my_germ$age)
#converting the z-score standard amount to a t-score  
my_germ$amount_tstand <- standard_tscore(var1=my_germ$amount)

#checking the mean for our new standards 
mean(my_germ$age_tstand)
mean(my_germ$amount_tstand)
#checking the standard deviation of our new standards 
sd(my_germ$age_tstand)
sd(my_germ$amount_tstand)

summary(my_germ$age_tstand)


#creating a UDF to normalize data 
normal <- function(var1){
  my_normal <-(var1-min(var1))/(max(var1)-min(var1))
  return(my_normal)  
}#closing the normal UDF

##these are unit-less variables (range from zero to one-- you can use these for predecitve analysis)
#since they have no units, you can compare them side by side 
my_germ$checking_norm <- normal(var1=my_germ$checking)
my_germ$duration_norm <- normal(var1=my_germ$duration)
my_germ$amount_norm <- normal(var1=my_germ$amount)
my_germ$employed_norm <- normal(var1=my_germ$employed)
my_germ$installp_norm <- normal(var1=my_germ$installp)
my_germ$age_norm <- normal(var1=my_germ$age)
my_germ$existcr_norm <- normal(var1=my_germ$existcr)
my_germ$telephon_norm <- normal(var1=my_germ$telephon)
my_germ$savings_norm <- normal(var1=my_germ$savings)
my_germ$coapp_norm <- normal(var1=my_germ$coapp)

#creating training and testing data sets

#sampling the indixes of my observations 
train_index <- sample(1:nrow(my_germ), size=0.8*nrow(my_germ))

#want to create a data set with these new customers 
#can create the train environment 
germ_train <- my_germ[train_index,]
#can create a test environment from subtracting the train environment 
germ_test <- my_germ[-train_index,]

#creating a predictive model- linear regression 

my_linear <- lm(amount~age,data=germ_train)
summary(my_linear)


#calculating the exp 
exp(0.2)-1
exp(-0.08)-1
exp(0.9)-1

#creating logistic regression model for data with units 
#(you intrepret this differently than the unit-less model)

#use glm function to predict our binary 
my_logit <- glm(binary~checking+duration+age+telephon+amount+savings+
                  installp+coapp, data=germ_train, family="binomial")


summary(my_logit)
#telephon value is too high so we need to remove 
#professor uses 0.1 as the statistical significance (takes out anything above it as not statistically significant for p-value)
#but maybe you could keep something about 0.1 if it makes business sense to keep the variable (even if it could be considered insignificat)

#if you see stars keep them in the model 

#lm function does not run logistic regressions (only linear), whereas glm function runs logistic regressions 

#checking the exp (for every additional unit of xxx the ods of our business success (increase/decrease) by 0.94)
exp(-0.02639)-1



my_logit_better <- glm(binary~checking+duration+age+amount+savings+
                  installp+coapp, data=germ_train, family="binomial")


summary(my_logit_better)

##my test to take out coapp since it is not statistically significant (pvalue too high)
my_logit_better_mine <- glm(binary~checking+duration+age+amount+savings+
                  installp, data=germ_train, family="binomial")


summary(my_logit_better_mine)

#designing a unitless logistic regression 

my_logit_norm <- glm(binary~checking_norm+duration_norm+age_norm+amount_norm+savings_norm
                      +installp_norm, data=germ_train, family = "binomial")

summary(my_logit_norm)

#do not convert the unitless data to exp-- the same framework will not work for it already be normalized 
#for the normalized data just compare side by side 

#duration has the strongest negative impact on the business success 

##session 8- performance eval

#install.packages("caret")
library(caret)

my_prediction_testing <- predict(my_logit, germ_test, type ="response")

confusionMatrix(data= as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference= as.factor(as.numeric(germ_test$binary)))


#creating confusion matric for training data 

my_prediction_training <- predict(my_logit, germ_train, type ="response")

confusionMatrix(data= as.factor(as.numeric(my_prediction_training > 0.5)),
                reference= as.factor(as.numeric(germ_train$binary)))

#creating the AUC and ROC framework 
#install.packages("ROCR")
library(ROCR)

my_prediction <- my_prediction_training

#prediction value logistic 
pred_val_logit <- prediction(my_prediction, germ_train$binary)

perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit)

#'the pig' anything below the line is bad, anything above is okay/good 
#easier to understand when you put a second chart on it 
##this is only good at comparing multiple models-- use confusion matrix for only one model


#creating a challenger decision tree for german credit card data 
library(rpart)
library(rpart.plot)

my_tree <- rpart(binary~checking+duration+age+telephon+amount+savings+
                  installp+coapp, data=germ_train, method = "class",
                 cp= 0.011)

rpart.plot(my_tree, extra=1, type=1)


#session 9 comparing model performance
my_tree_predict_test <- predict(my_tree, germ_test, type="prob")
my_tree_predict_train <- predict(my_tree, germ_train, type="prob")

#the object has two variables: 1 business failure, 2 business success: we want the
#business success probability therefore we use [,2]
my_tree_prediction <- prediction(my_tree_predict_train[,2], germ_train$binary)

my_tree_performance <- performance(my_tree_prediction, "tpr", "fpr")
print("AUC: ", my_tree_performance@y.values)

plot(my_tree_performance, col="black")
plot(perf_logit, col="purple", add=TRUE)



#the purple plot is better because: tend to look at the left-hand chart more than the right-hand side
#want the low false-positive rates more rather than right false-positive rates 
#the two models are very similar and similar performance 
#both checking and duration were the fav- very strong positive/negative impact 
#good splitting power 
#logistic is just slightly better 

#piping property and selecting savings and checking 
my_germ %>%
  filter(property >3) %>%
  select(checking, savings)














