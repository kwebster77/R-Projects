##########################################
####### Created by Karley Webster
######### MBAN2 Hult 2021
############## Date: 10.24.2021
############### Homework 1.0
##################Version 0.1
######################Exercise 1, Question 2
##########################################

#####Exercise 2, Question 3
#####installing the package (xxx) using the library function to run the package and 
#the ? function to get help on the library
install.packages("neuralnet")
library(neuralnet)
?`neuralnet-package`
?neuralnet
##### 'neuralnet-package' provides us with the documentation of the library 
##### while neuralnet provides us with the usage and arguments for using the libaray

#####Exercise 3, Question 8
##creating a user defined function
## First, creating the transform_matrix from exercise 7
transform_vector <- c(10, 11, 9, 15, 19, 52, 19, 7, 10, 22, 28, 40, 6, 99, 33, 35, 26, 5, 87, 91, 0, 12, 16, 81, 200)
transform_matrix <- matrix(transform_vector, nrow = 5 , ncol = 5, byrow = TRUE)
View(transform_matrix)

##then, creating the transform_matrix_example from 2.1 
transform_vector_example <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
transform_matrix_example <- matrix(transform_vector_example, nrow = 3 , ncol = 3, byrow = TRUE)
View(transform_matrix_example)

##Then, using the transform_matrix to create a UDF for the diagonal printing the mean and sd

transform_function <- function(x) {
  try(diag(x))
  try(my_mu <- mean(x, na.rm = TRUE)) 
  try(my_sd <- sd(x, na.rm = TRUE)) 
  try(return(c(my_mu, my_sd)))
}  #closing the UDF

#calling the function for the transform_matrix from exercise 7 and printing
transform_matrix_results <- transform_function(transform_matrix)
print(transform_matrix_results)

#calling the function for the transform_matrix_example from 2.1 and printing
transform_example_results <- transform_function(transform_matrix_example)
print(transform_example_results)


#####Exercise 4, Question 10
?complete.cases
##creating a cleanup function to input a data set and column index which returns all variables without missing values
#using the function complete.cases would be more efficient than using a loop on the data
cleanup_function <- function(dataset, col_index) {
  new_df <- dataset[complete.cases(dataset), col_index]
  new_df
} #closing the function

##testing using air quality data set from MASS- first input the data set, then use the cleanup function created above
library(MASS)
cleanup_function(airquality, col_index=c(2:3))


###Exercise 4, Question 10
##try 2-- using a loop 
clean_a_fun <- function(dataset, col_index) {
  new_dataset <- dataset
  for (i in 1:length(col_index)) {
   clean_column <- new_dataset[-which(is.na(new_dataset[,col_index[i]])),]
   if(nrow(clean_column) !=0) {
     new_dataset <- clean_column
   }
  }
  return(new_dataset)
}

clean_a_fun(airquality, col_index = c(3:5))










