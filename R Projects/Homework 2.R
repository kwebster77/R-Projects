####################################
############Hult International Business School 
############Karley Webster
############ MSBAN Homework 2
############ 11/02/2021
###################################

###############
####Exercise 3
##############

#combining the dataset and changing the data to binomials 
print(iris)
sapply(iris, function(x) sum(is.na(x)))
iris$group <- ifelse(iris$Species == "virginica", 1, 0)
is.numeric(iris$group)

#train the model 
train_index <- sample(1:nrow(iris), size=0.8*nrow(iris))
iris_train <- iris[train_index,]
iris_test <- iris[-train_index,]

#import libraries
library(ggplot2)
library(GGally)
#use ggpairs to identify how the variables are related to each other 
ggpairs(iris_train)

#create a logistic regression 
iris_logit <- glm(group~Sepal.Length+Sepal.Width+Petal.Width+ 
                         Petal.Length, data=iris_train, family = "binomial")
summary(iris_logit)


#predict a plant 
plant1 <- data.frame(
  Sepal.Length = 9,
  Sepal.Width= 5,
  Petal.Length = 5,
  Petal.Width = 2
)

predict(iris_logit, plant1, type = "response")


###############
####Exercise 4
##############

#install.packages("rpart")
library(rpart)
#print the dataset 
print(kyphosis)
#create a new category- to change Kyphosis into binomals (don't write over original dataset, hence the new column)
kyphosis$Status <- ifelse(kyphosis$Kyphosis == "present", 1, 0)
#check to ensure the new status is numeric 
is.numeric(kyphosis$Status)

#train the model 
train_index_kyphosis <- sample(1:nrow(kyphosis), size=0.8*nrow(kyphosis))
kyphosis_train <- kyphosis[train_index_kyphosis,]
kyphosis_test <- kyphosis[-train_index_kyphosis,]

#create a logistic regression 
kyphosis_logit <- glm(Status~Age+Number+Start, data=kyphosis_train, family = "binomial")
summary(kyphosis_logit)

#predict a presence of kyphosis
kyphosis1 <- data.frame(
  Age = 50,
  Number= 5,
  Start = 10
)

predict(kyphosis_logit, kyphosis1, type = "response")

###############
####Exercise 5
##############

#exercise 1
#create a linear regression for sepal length and sepal width
iris_linear_sepal_width <- lm(Sepal.Length~Sepal.Width, data = iris_train)
summary(iris_linear_sepal_width)

#create a linear regression for sepal length and petal length
iris_linear_petal_length <- lm(Sepal.Length~Petal.Length, data = iris_train)
summary(iris_linear_petal_length)

#create a linear regression for sepal length and petal width
iris_linear_petal_width <- lm(Sepal.Length~Petal.Width, data = iris_train)
summary(iris_linear_petal_width)

library(ggplot2)
plot(x=iris$Sepal.Width, y=iris$Sepal.Length,type="p")
plot(x=iris$Petal.Length, y=iris$Sepal.Length,type="p")
plot(x=iris$Petal.Width, y=iris$Sepal.Length,type="p")

#running a Breush Pagan Test
#install.packages("lmtest")
library(lmtest)

#conducting a Breusch-Pagan Test to determine if 
#heteroscedasticity is present in a regression analysis
bptest(iris_linear_petal_length)
bptest(iris_linear_petal_width)
bptest(iris_linear_sepal_width)







