##### Class: Text Analytics and Natural Language Processing 
##### Karley Webster 
##### MBAN2 HULT 2021
##### Piping
##### Version 0.1

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







