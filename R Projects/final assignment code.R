#######################################
############ Created by Karley Webster
######### MBAN2 Hult 2021
############## Date: 11.09.2021
######### Exam Version 0.1
##########################################

library(mlbench)
library(dplyr)

summary(BreastCancer)

sapply(BreastCancer, function(x) sum(is.na(x)))

#creating an empty vector
BreastCancer$status <-c()

#need to reclassify as character and numeric first 
BreastCancer$Class <- as.character(BreastCancer$Class)
BreastCancer$Mitoses <- as.numeric(BreastCancer$Mitoses)

#creating a for loop to relabel

for (i in 1:nrow(BreastCancer)) {
  if (BreastCancer$Class[i] == "malignant" & BreastCancer$Mitoses[i] > 1) {
    BreastCancer$status[i] <- "critical"
  } else {
    BreastCancer$status[i] <- "normal"
  }
}


BreastCancer$status_binary <- ifelse(BreastCancer$status == "normal", 1, 0)

sum(BreastCancer$status_binary)

status_pie <- c("critical", "normal")
Patients_pie <- c((107/699)*100,(592/699)*100)
status_percent <- paste(status_pie, Patients_pie,"%")
pie(Patients_pie, status_percent, main="Critical vs. Normal Patients")


##################Question 2
#lambda would be 1/mean = 1/5 = 0.2
#run exponential distribution: 

my_exp <- rexp(10000, rate = 0.2)
mean(my_exp)
hist(my_exp)

1-exp(-0.2*1)
1-exp(-0.2*8)
exp(-0.2*10)

##dying in less than 1 day= 0.1812692
##dying in less than 8 days= 0.7981035
##living more than 10 days= 0.1353353

###################Question 3

library(rpart)
library(rpart.plot)
library(caret)

#sampling the indicies from the observations
#took 80% for training index as this follow the 80/20
train_index <- sample(1:nrow(BreastCancer), size=0.8*nrow(BreastCancer))

#can create the train environment 
BreastCancer_train <- BreastCancer[train_index,]
#can create a test environment from subtracting the train environment 
BreastCancer_test <- BreastCancer[-train_index,]


#creating a Logistic regression: 

my_logit <- glm(status_binary~Cl.thickness+Cell.size+Cell.shape+Marg.adhesion+Epith.c.size
                +Bare.nuclei+Bl.cromatin+Normal.nucleoli, data=BreastCancer_train, family="binomial")


summary(my_logit)

my_prediction_testing <- predict(my_logit, BreastCancer_test, type ="response")

confusionMatrix(data= as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference= as.factor(as.numeric(BreastCancer_test$status_binary)))


#creating confusion matric for training data 

my_prediction_training <- predict(my_logit, BreastCancer_train, type ="response")

confusionMatrix(data= as.factor(as.numeric(my_prediction_training > 0.5)),
                reference= as.factor(as.numeric(BreastCancer_train$status_binary)))


##Creating a decision tree based on the train and test index (without rescaling variables)
my_tree <- rpart(status_binary~Cl.thickness+Cell.size+Cell.shape+Marg.adhesion+Epith.c.size
                 +Bare.nuclei+Bl.cromatin+Normal.nucleoli, data=BreastCancer_train, method = "class",
                 cp= 0.025)

rpart.plot(my_tree, extra=1, type=1)






