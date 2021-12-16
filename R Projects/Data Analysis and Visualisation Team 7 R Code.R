####################################
############Hult International Business School 
############MSBAN Team 7
############ Data Analysis and Visualization Team Assignment
############ 11/14/2021
###################################

library(readxl)

datasets_marketing_campaign_SF_1_ <- read_excel("C:/Users/gmith/OneDrive/Desktop/Mithu/MSBA/Data Analytics/Final Project/datasets_marketing_campaign_SF (1).xlsx")
View(datasets_marketing_campaign_SF_1_)

datasets_marketing_campaign_SF <- read_excel("Documents/Education/Data Science- R/Team Project 2/datasets_marketing_campaign_SF.xlsx", 
                                             col_types = c("numeric", "numeric", "text", 
                                                           "text", "numeric", "numeric", "numeric", 
                                                           "text", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", 
                                                           "text"))
View(datasets_marketing_campaign_SF)

campaign <- datasets_marketing_campaign_SF_1_
campaign<- datasets_marketing_campaign_SF

##libraries 
library(ggplot2)
library(plotly)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)


##################################
#################Understanding the Data
##################################
nrow(campaign)
ncol(campaign) 
colnames(campaign) 

#checking for missing data
sapply(campaign, function(x) sum(is.na(x)))


##################################
#################Cleaning the Data
##################################

campaign_new <- campaign

#cleaning the blanks 
campaign_new <- campaign[-which(is.na(campaign$Income)),]

#formatting dates into date category 
campaign_new$Dt_Customer <- as.Date(campaign_new$Dt_Customer, format = "%Y-%m-%d")

#creating new column for age 
for (i in 1:nrow(campaign_new)) {
  campaign_new$Age[i] <- 2021-campaign_new$Year_Birth[i]
}

#creating bins for ages 
max(campaign_new$Year_Birth)
min(campaign_new$Year_Birth)
range(campaign_new$Year_Birth)
summary(campaign$Year_Birth)
campaign_new$age_bin[campaign_new$Year_Birth <= 1959] <- 'Senior'
campaign_new$age_bin[campaign_new$Year_Birth > 1959 & campaign_new$Year_Birth <= 1970] <- 'Middle_Age'
campaign_new$age_bin[campaign_new$Year_Birth > 1970 & campaign_new$Year_Birth <= 1977] <- 'Adult'
campaign_new$age_bin[campaign_new$Year_Birth > 1977 & campaign_new$Year_Birth <= 1996 ] <- 'Young_Adult'

#Grouping US and rest of world 
campaign_new$country_dummy <- ifelse(campaign_new$Country == "US", 1, 0)

#creating a total purchases category 
campaign_new$total_purchases <- campaign_new$NumDealsPurchases + campaign_new$NumCatalogPurchases +campaign_new$NumStorePurchases + campaign_new$NumWebPurchases


#identifying outliers 
boxplot(campaign_new$total_purchases,
                        ylab = "total purchases")
boxplot(campaign_new$Income,
                     ylab = "Income")
boxplot(campaign_new$Recency, 
                       ylab= "Recency")
boxplot(campaign_new$MntWines,
                   ylab = "Amount Wines")
boxplot(campaign_new$MntFruits,
                    ylab = "Amount Fruits")
boxplot(campaign_new$MntMeatProducts,
                   ylab = "Amount Meat")
boxplot(campaign_new$NumCatalogPurchases,
                      ylab = "Catalog Purchases")
boxplot(campaign_new$NumWebPurchases,
                  ylab = "Web Purchases")
boxplot(campaign_new$NumStorePurchases,
                    ylab = "Store Purchases")
boxplot(campaign_new$NumWebVisitsMonth,
                         ylab = "Web Visits")

##removing outliers

campaign_no_outliers <- campaign_new

#for loop to remove variables which are well above or below the IQR
remove_outliers <- function(x, na.rm = TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

campaign_no_outliers$NumWebPurchases <- remove_outliers(campaign_no_outliers$NumWebPurchases)
campaign_no_outliers$Income <- remove_outliers(campaign_no_outliers$Income)
campaign_no_outliers$Recency <- remove_outliers(campaign_no_outliers$Recency)
campaign_no_outliers$MntWines <- remove_outliers(campaign_no_outliers$MntWines)
campaign_no_outliers$MntFruits <- remove_outliers(campaign_no_outliers$MntFruits)
campaign_no_outliers$MntMeatProducts <- remove_outliers(campaign_no_outliers$MntMeatProducts)
campaign_no_outliers$MntFishProducts <- remove_outliers(campaign_no_outliers$MntFishProducts)
campaign_no_outliers$MntSweetProducts <- remove_outliers(campaign_no_outliers$MntSweetProducts)
campaign_no_outliers$NumDealsPurchases <- remove_outliers(campaign_no_outliers$NumDealsPurchases)
campaign_no_outliers$NumCatalogPurchases <- remove_outliers(campaign_no_outliers$NumCatalogPurchases)
campaign_no_outliers$NumWebVisitsMonth <- remove_outliers(campaign_no_outliers$NumWebVisitsMonth)

##################################
#################Part I Questions 
##################################

##Question B -with outliers 

## linear Regression
hist(campaign_new$total_purchases)
plotNormalHistogram(campaign_new$total_purchases)

#transforming to Sqrt Transformation
campaign_new$Sqrt_transform <- sqrt(campaign_new$total_purchases)
plotNormalHistogram(campaign_new$Sqrt_transform)

##Creating a ggplot 
chart1 <- ggplot(data=campaign_new, aes(x=country_dummy, y=total_purchases, color=NumWebVisitsMonth)) + geom_jitter()
ggplot(data=campaign_new, aes(x=country_dummy, y=Sqrt_transform, color=NumWebVisitsMonth)) + geom_jitter()
ggplotly(chart1)
#running a linear regression

#running a single regression between countries and total purchases
linear_country <- lm(total_purchases ~ country_dummy, data = campaign_new)
summary(linear_country)
plot(linear_country_total)

#running a transformation between countries and total purchases 
linear_country2 <- lm(Sqrt_transform ~ country_dummy, data = campaign_new)

summary(linear_country2)

plot(linear_country2)

plot(density(residuals(linear_country2)))

##running an MRM using dummy variables--with outliers

campaign_new$country_deals <- campaign_new$country_dummy*campaign_new$NumDealsPurchases
campaign_new$country_web <- campaign_new$country_dummy*campaign_new$NumWebPurchases
campaign_new$country_catalog <- campaign_new$country_dummy*campaign_new$NumCatalogPurchases
campaign_new$country_store <- campaign_new$country_dummy*campaign_new$NumStorePurchases

linear_country_total_no_outliers <- lm(total_purchases~ NumDealsPurchases
                                       +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+country_deals
                                       +country_web+country_catalog+country_store
                                       +country_dummy, data = campaign_new)

summary(linear_country_total_no_outliers)
plot(linear_country_total_no_outliers)

##Question B without outliers 
#creating a total purchases category 
campaign_no_outliers$total_purchases <- campaign_no_outliers$NumDealsPurchases + campaign_no_outliers$NumCatalogPurchases +campaign_no_outliers$NumStorePurchases + campaign_no_outliers$NumWebPurchases
plotNormalHistogram(campaign_no_outliers$total_purchases)
chart2 <- ggplot(data=campaign_no_outliers, aes(x=country_dummy, y=total_purchases, color=NumWebVisitsMonth)) + geom_jitter()
ggplotly(chart2)

linear_country_no_outliers <- lm(total_purchases ~ country_dummy, data = campaign_no_outliers)
summary(linear_country_no_outliers)


##running an MRM using dummy variables--no outliers 

campaign_no_outliers$country_deals <- campaign_no_outliers$country_dummy*campaign_no_outliers$NumDealsPurchases
campaign_no_outliers$country_web <- campaign_no_outliers$country_dummy*campaign_no_outliers$NumWebPurchases
campaign_no_outliers$country_catalog <- campaign_no_outliers$country_dummy*campaign_no_outliers$NumCatalogPurchases
campaign_no_outliers$country_store <- campaign_no_outliers$country_dummy*campaign_no_outliers$NumStorePurchases

linear_country_total_no_outliers <- lm(total_purchases~ NumDealsPurchases
                                       +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+country_deals
                                       +country_web+country_catalog+country_store
                                       +country_dummy, data = campaign_no_outliers)

summary(linear_country_total_no_outliers)
plot(linear_country_total_no_outliers)

########Question C

## Gold and In Store Purchases for loop
for(i in 1:nrow(campaign_new)) {
  if(campaign_new$MntGoldProds[i] > mean(campaign_new$MntGoldProds, na.rm = TRUE)) {
    campaign_new$gold_dummy[i] <- 1
  } else {
    campaign_new$gold_dummy[i] <- 0
  }
}#closing the for loop

# Creating interaction variable
campaign_new$fish_int <- campaign_new$gold_dummy*campaign_new$MntFishProducts
linear_gold_instore <- lm(NumStorePurchases ~ gold_dummy+MntFishProducts+fish_int, data = campaign_new)
summary(linear_gold_instore)

########Question D

###Step 1: We create dummy variables for PhD and Married. 
campaign_new$PhD <- ifelse(grepl("PhD",campaign_new$Education)==TRUE,1,0)
campaign_new$Married <- ifelse(grepl("Married",campaign_new$Marital_Status)==TRUE,1,0)

#LRM for PhD and Married
model_Phd <- lm(MntFishProducts ~ PhD,
                data = campaign_new )
summary(model_Phd)

model_Married <- lm(MntFishProducts ~ Married,
                    data = campaign_new )
summary(model_Married)

model_Phd_Married <- lm(MntFishProducts ~ PhD+Married,
                        data = campaign_new )
summary(model_Phd_Married)

##Step 2: Our interaction variable will be PhD*Married. I will do this directly in the MR
#Glm is when our dependent variable is binary. 

model_interaction_1 <- lm(MntFishProducts ~ PhD+Married+PhD*Married,
                          data = campaign_new )
summary(model_interaction_1)

cor(campaign_new$PhD,campaign_new$MntFishProducts)
cor(campaign_new$Married,campaign_new$MntFishProducts)

#Looking other important variables. 
#Correlation of kidhombe & Teenhome 
cor(campaign_new$Kidhome,campaign_new$Teenhome) #[1] -0.03986909, they have a negative correlation. 

# I suggest that our model should have demographic variables and variable related to the company action or performance.

model_a <- lm(MntFishProducts ~ AcceptedCmp1+AcceptedCmp2+AcceptedCmp3+AcceptedCmp4+AcceptedCmp5,
              data = campaign_new )
summary(model_a)

model_b <- lm(MntFishProducts ~ PhD+Married+ Income + Kidhome + Teenhome,
              data = campaign_new )
summary(model_b)

model_c <- lm(MntFishProducts ~ PhD+Married+ Income + Kidhome + Teenhome + AcceptedCmp1 + AcceptedCmp5,
              data = campaign_new )
summary(model_c)

#Best model
model_d <- lm(MntFishProducts ~ PhD+ Income + Kidhome + Teenhome + AcceptedCmp1 ,
              data = campaign_new )
summary(model_d)

########Question e

# Run regression to test which campaign has resulted in better purchases

1# Campaign 1

campaign_new$AcceptedCmp1_intdeals <- campaign_new$AcceptedCmp1*campaign_new$NumDealsPurchases
campaign_new$AcceptedCmp1_intweb <- campaign_new$AcceptedCmp1*campaign_new$NumWebPurchases
campaign_new$AcceptedCmp1_intcatalog <- campaign_new$AcceptedCmp1*campaign_new$NumCatalogPurchases
campaign_new$AcceptedCmp1_intstore <- campaign_new$AcceptedCmp1*campaign_new$NumStorePurchases

linear_camp1 <- lm(NumWebPurchases~ NumDealsPurchases
                   +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+AcceptedCmp1
                   +AcceptedCmp1_intdeals+AcceptedCmp1_intweb+AcceptedCmp1_intstore
                   +AcceptedCmp1_intcatalog, data = campaign_new)

summary(linear_camp1)

2# Campaign 2

campaign_new$AcceptedCmp2_intdeals <- campaign_new$AcceptedCmp2*campaign_new$NumDealsPurchases
campaign_new$AcceptedCmp2_intweb <- campaign_new$AcceptedCmp2*campaign_new$NumWebPurchases
campaign_new$AcceptedCmp2_intcatalog <- campaign_new$AcceptedCmp2*campaign_new$NumCatalogPurchases
campaign_new$AcceptedCmp2_intstore <- campaign_new$AcceptedCmp2*campaign_new$NumStorePurchases

linear_camp2 <- lm(NumWebPurchases~ NumDealsPurchases
                   +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+AcceptedCmp2
                   +AcceptedCmp2_intdeals+AcceptedCmp2_intweb+AcceptedCmp2_intstore
                   +AcceptedCmp2_intcatalog, data = campaign_new)

summary(linear_camp2)

3# Campaign 3

campaign_new$AcceptedCmp3_intdeals <- campaign_new$AcceptedCmp3*campaign_new$NumDealsPurchases
campaign_new$AcceptedCmp3_intweb <- campaign_new$AcceptedCmp3*campaign_new$NumWebPurchases
campaign_new$AcceptedCmp3_intcatalog <- campaign_new$AcceptedCmp3*campaign_new$NumCatalogPurchases
campaign_new$AcceptedCmp3_intstore <- campaign_new$AcceptedCmp3*campaign_new$NumStorePurchases

linear_camp3 <- lm(NumWebPurchases~ NumDealsPurchases
                   +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3
                   +AcceptedCmp3_intdeals+AcceptedCmp3_intweb+AcceptedCmp3_intstore
                   +AcceptedCmp3_intcatalog, data = campaign_new)

summary(linear_camp3)

4# Campaign 4

campaign_new$AcceptedCmp4_intdeals <- campaign_new$AcceptedCmp4*campaign_new$NumDealsPurchases
campaign_new$AcceptedCmp4_intweb <- campaign_new$AcceptedCmp4*campaign_new$NumWebPurchases
campaign_new$AcceptedCmp4_intcatalog <- campaign_new$AcceptedCmp4*campaign_new$NumCatalogPurchases
campaign_new$AcceptedCmp4_intstore <- campaign_new$AcceptedCmp4*campaign_new$NumStorePurchases
campaign_new$AcceptedCmp4_intvisit <- campaign_new$AcceptedCmp4*campaign_new$NumWebVisitsMonth

linear_camp4 <- lm(NumWebPurchases~ NumDealsPurchases
                   +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+AcceptedCmp4
                   +AcceptedCmp4_intdeals+AcceptedCmp4_intweb+AcceptedCmp4_intstore
                   +AcceptedCmp4_intcatalog, data = campaign_new)

summary(linear_camp4)

5 #Campaign 5

campaign_new$AcceptedCmp5_intdeals <- campaign_new$AcceptedCmp5*campaign_new$NumDealsPurchases
campaign_new$AcceptedCmp5_intweb <- campaign_new$AcceptedCmp5*campaign_new$NumWebPurchases
campaign_new$AcceptedCmp5_intcatalog <- campaign_new$AcceptedCmp5*campaign_new$NumCatalogPurchases
campaign_new$AcceptedCmp5_intstore <- campaign_new$AcceptedCmp5*campaign_new$NumStorePurchases
campaign_new$AcceptedCmp5_intvisit <- campaign_new$AcceptedCmp5*campaign_new$NumWebVisitsMonth

linear_camp5 <- lm(NumWebPurchases~ NumDealsPurchases
                   +NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth+AcceptedCmp5
                   +AcceptedCmp5_intdeals+AcceptedCmp5_intweb+AcceptedCmp5_intstore
                   +AcceptedCmp5_intcatalog, data = campaign_new)

summary(linear_camp5)

6 #Last campaign

campaign_new$Response_intdeals <- campaign_new$Response*campaign_new$NumDealsPurchases
campaign_new$Response_intweb <- campaign_new$Response*campaign_new$NumWebPurchases
campaign_new$Response_intcatalog <- campaign_new$Response*campaign_new$NumCatalogPurchases
campaign_new$Response_intstore <- campaign_new$Response*campaign_new$NumStorePurchases

linear_camp6 <- lm(NumWebPurchases ~ NumDealsPurchases+NumStorePurchases+NumCatalogPurchases
                   +NumWebVisitsMonth+Response+Response_intweb+
                     +Response_intstore+Response_intcatalog+Response_intdeals, data = campaign_new)
summary(linear_camp6)


#####################################################################################################################
######################################################Part II Questions 
#####################################################################################################################

# Successful campaign

Camp1 <- sum(campaign_new$AcceptedCmp1)
Camp2 <- sum(campaign_new$AcceptedCmp2)
Camp3 <- sum(campaign_new$AcceptedCmp3)
Camp4 <- sum(campaign_new$AcceptedCmp4)
Camp5 <- sum(campaign_new$AcceptedCmp5)
Camp6 <- sum(campaign_new$Response)

camps <- c(Camp1, Camp2, Camp3, Camp4, Camp5, Camp6)
camp_labl <- c("Camp1", "Camp2", "Camp3", "Camp4", "Camp5", "Camp6")

Fig_7<- plot_ly(campaign_new, x=camp_labl, y=camps, type = "bar", marker = list(color = c("silver","silver","silver",
                                                                                          "silver","silver", "red"))) %>% layout(title = '<b>Campaign success</b>')
Fig_7

# Region wise success

Country_wise <- campaign_new %>% 
  group_by(Country) %>%
  summarise(sum(AcceptedCmp1), sum(AcceptedCmp2), sum(AcceptedCmp3), sum(AcceptedCmp4), sum(AcceptedCmp5), sum(Response))

Country_wise <- as.data.frame(Country_wise)
Country_wise$total <- Country_wise$`sum(AcceptedCmp1)`+Country_wise$`sum(AcceptedCmp2)`+Country_wise$`sum(AcceptedCmp3)`+
  Country_wise$`sum(AcceptedCmp4)`+Country_wise$`sum(AcceptedCmp5)`+Country_wise$`sum(Response)`

colnames(Country_wise) <- c("Country", "Campaign1", "Campaign2", "Campaign3","Campaign4","Campaign5","Campaign6", "Total")


Country_wise_bar <- pivot_longer(Country_wise, 
                                 cols=!c(`Country`,`Total`),
                                 names_to='Campaigns', 
                                 values_to='Acceptance')

Fig_8 <- ggplot(Country_wise_bar, aes(x=`Country`, y=Acceptance, fill=Campaigns)) +
  geom_bar(stat='identity', position='dodge')+
  scale_fill_manual(values=c("Campaign1" = "#AFB1A9", "Campaign2" = "#A2A39D", "Campaign3" = "#646658", 
                             "Campaign4" = "#C70039", "Campaign5" = "#900C3F", "Campaign6" = "#620E05"))
Fig_8

# Successful mode

Deals <- sum(campaign_new$NumDealsPurchases)
Catalog <- sum(campaign_new$NumCatalogPurchases)
Web <- sum(campaign_new$NumWebPurchases)
Store <- sum(campaign_new$NumStorePurchases)
Total_Purchase <- Deals+Catalog+Web+Store

Modes <- c(round(Deals/Total_Purchase*100), 
           round(Catalog/Total_Purchase*100), 
           round(Web/Total_Purchase*100), 
           round(Store/Total_Purchase*100))
modes_labl <- c("Deals", "Catalog", "Web", "Store")


modes_percent <- paste(modes_labl, Modes,"%")

modes_pie <- pie(Modes,modes_percent, main = "Different modes of purchase", col = c("#AFB1A9","#900C3F","#A2A39D","#C70039"))

# Successful product

Fruits <- sum(campaign_new$MntFruits)
Wines <- sum(campaign_new$MntWines)
Sweet <- sum(campaign_new$MntSweetProducts)
Gold <- sum(campaign_new$MntGoldProds)
Meat <- sum(campaign_new$MntMeatProducts)
Fish <- sum(campaign_new$MntFishProducts)

Total_Spend_overall <- Fruits+Wines+Sweet+Gold+Meat+Fish

Spend_overall <- c(round(Fruits/Total_Spend_overall*100), 
                   round(Wines/Total_Spend_overall*100), 
                   round(Gold/Total_Spend_overall*100), 
                   round(Sweet/Total_Spend_overall*100),
                   round(Meat/Total_Spend_overall*100),
                   round(Fish/Total_Spend_overall*100))
spend_labl_overall <- c("Fruits", "Wines", "Gold", "Sweet", "Meat", "Fish")


spend_percent_overall <- paste(spend_labl_overall, Spend_overall,"%")
total_purchase_pie_chart <- pie(Spend_overall,spend_percent_overall, main = "Product wise amount spent",col = c("#AFB1A9","#C70039","#A2A39D","#900C3F", "#646658", "#620E05"))

# Customer profile 

agt_matrix_fruits <- as.data.frame(aggregate(campaign_new$MntFruits,
                                             list(campaign_new$Education), FUN = sum))
names(agt_matrix_fruits)[2] <- "Amtfruit"

agt_matrix_sweet <- as.data.frame(aggregate(campaign_new$MntSweetProducts,
                                            list(campaign_new$Education), FUN = sum))

Fig_9 <- plot_ly(campaign_new, x=agt_matrix_fruits$Group.1, y=agt_matrix_fruits$Amtfruit, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                                "silver","silver"))) %>% layout(title = '<b>Education level vs Fruit purchases</b>')
Fig_9

Fig_10 <- plot_ly(campaign_new, x=agt_matrix_sweet$Group.1, y=agt_matrix_sweet$x, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                        "silver","silver"))) %>% layout(title = '<b>Education level vs Sweet purchases</b>')
Fig_10

agt_matrix_age_bin_fruits <- as.data.frame(aggregate(campaign_new$MntFruits,
                                                     list(campaign_new$age_bin), FUN = sum))


Fig_11<- plot_ly(campaign_new, x=agt_matrix_age_bin_fruits$Group.1, y=agt_matrix_age_bin_fruits$x, type = "bar", marker = list(color = c("silver","silver","red",                                                                                                                                       "silver"))) %>% layout(title = '<b>Age bucket vs Fruit purchases</b>')

Fig_11

agt_matrix_age_bin_sweet <- as.data.frame(aggregate(campaign_new$MntSweetProducts,
                                                    list(campaign_new$age_bin), FUN = sum))


Fig_12<- plot_ly(campaign_new, x=agt_matrix_age_bin_sweet$Group.1, y=agt_matrix_age_bin_sweet$x, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                                       "silver"))) %>% layout(title = '<b>Age bucket vs Sweet purchases</b>')
Fig_12

# Teen home consumption

# Creating dummy variable for homes with teenagers

for (i in 1:nrow(campaign_new)) {
  if(campaign_new$Teenhome[i] > 0) {
    campaign_new$teen[i] <- "Yes"
  } else {
    campaign_new$teen[i] <- "No"
  }
}#closing the for loop

# for loops for total of teen home consumption

## Fruits
teen_fruits_pur <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_fruits_pur <- c(teen_fruits_pur,campaign_new$MntFruits[i])
  }
  teen_fruits_total <- sum(teen_fruits_pur)
}#closing the for loop

## Meat
teen_meat <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_meat <- c(teen_meat,campaign_new$MntMeatProducts[i])
  }
  teen_meat_total <- sum(teen_meat)
}#closing the for loop

## Fish
teen_fish <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_fish <- c(teen_fish,campaign_new$MntFishProducts[i])
  }
  teen_fish_total <- sum(teen_fish)
}#closing the for loop

## Wines
teen_wine_pur <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_wine_pur <- c(teen_wine_pur,campaign_new$MntWines[i])
  }
  teen_wine_total <- sum(teen_wine_pur)
}#closing the for loop

## Sweet
teen_sweet_pur <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_sweet_pur <- c(teen_sweet_pur,campaign_new$MntSweetProducts[i])
  }
  teen_sweet_total <- sum(teen_sweet_pur)
}#closing the for loop

## Gold
teen_gold_pur <- c()
for (i in 1:nrow(campaign_new)) {
  if(campaign_new$teen[i] == "Yes") {
    teen_gold_pur <- c(teen_gold_pur,campaign_new$MntGoldProds[i])
  }
  teen_gold_total <- sum(teen_gold_pur)
}#closing the for loop

total_spend <- teen_fish_total+teen_fruits_total+teen_gold_total+teen_wine_total+teen_sweet_total+teen_meat_total

spend_teen <- c(round(teen_fish_total/total_spend*100), 
                round(teen_fruits_total/total_spend*100), 
                round(teen_gold_total/total_spend*100), 
                round(teen_wine_total/total_spend*100), 
                round(teen_sweet_total/total_spend*100), 
                round(teen_meat_total/total_spend*100))
labels <- c("Fish", "Fruits", "Gold", "Wine", "Sweet", "Meat")

label_percent <- paste(labels, spend_teen,"%")

total_pub_pie_chart <- pie(spend_teen,label_percent, main = "Families with teenagers spend",col = c("#AFB1A9","#C70039","#A2A39D","#900C3F", "#646658", "#620E05"))

# Correlation heatmap

# install.packages("ggcorrplot")
# Correlation between purchases

corr_mat_check <- campaign_new[,c(16,17,18,19,20)]
correlation_matrix <- round(cor(corr_mat_check), 2)

# Correlation between purchases

corr_mat_check2 <- campaign_new[,c(10,11,12,13,14,15)]
correlation_matrix2 <- round(cor(corr_mat_check2), 2)

ggcorrplot(correlation_matrix, colors = c("#E68C82", "white", "#620E05"))

ggcorrplot(correlation_matrix2, colors = c("#A5DBF0", "white", "#096AAB"))


#####################################################################################################################

######### Other Analysis #########

# Fish and Meat purchases graphs - qualitative variables

agt_matrix_fish <- as.data.frame(aggregate(campaign_new$MntFishProducts,
                                           list(campaign_new$Education), FUN = sum))
names(agt_matrix_fish)[2] <- "Amtfish"

agt_matrix_meat <- as.data.frame(aggregate(campaign_new$MntMeatProducts,
                                           list(campaign_new$Education), FUN = sum))

Fig_1 <- plot_ly(campaign_new, x=agt_matrix_fish$Group.1, y=agt_matrix_fish$Amtfish, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                           "silver","silver"))) %>% layout(title = "Education level vs Fish purchases")

print(Fig_1)

Fig_2 <- plot_ly(campaign_new, x=agt_matrix_meat$Group.1, y=agt_matrix_meat$x, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                     "silver","silver"))) %>% layout(title = "Education level vs Meat purchases")

print(Fig_2)

agt_matrix_fish_marital <- as.data.frame(aggregate(campaign_new$MntFishProducts,
                                                   list(campaign_new$Marital_Status), FUN = sum))



Fig_3 <- plot_ly(campaign_new, x=agt_matrix_fish_marital$Group.1, y=agt_matrix_fish_marital$x, type = "bar", marker = list(color = c("silver","silver","silver",
                                                                                                                                     "red","silver", "silver", "silver", "silver"))) %>% layout(title = "Marital status vs Fish purchases")
print(Fig_3)

agt_matrix_meat_marital <- as.data.frame(aggregate(campaign_new$MntMeatProducts,
                                                   list(campaign_new$Marital_Status), FUN = sum))



Fig_4<- plot_ly(campaign_new, x=agt_matrix_meat_marital$Group.1, y=agt_matrix_meat_marital$x, type = "bar", marker = list(color = c("silver","silver","silver",
                                                                                                                                    "red","silver", "silver", "silver", "silver"))) %>% layout(title = "Marital status vs Meat purchases")

print(Fig_4)

agt_matrix_age_bin <- as.data.frame(aggregate(campaign_new$MntFishProducts,
                                              list(campaign_new$age_bin), FUN = sum))


Fig_5<- plot_ly(campaign_new, x=agt_matrix_age_bin$Group.1, y=agt_matrix_age_bin$x, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                          "silver"))) %>% layout(title = "Age bucket vs Fish purchases")

print(Fig_5)

agt_matrix_age_bin_meat <- as.data.frame(aggregate(campaign_new$MntMeatProducts,
                                                   list(campaign_new$age_bin), FUN = sum))


Fig_6<- plot_ly(campaign_new, x=agt_matrix_age_bin_meat$Group.1, y=agt_matrix_age_bin_meat$x, type = "bar", marker = list(color = c("silver","silver","red",
                                                                                                                                    "silver"))) %>% layout(title = "Age bucket vs Meat purchases")

print(Fig_6)


# Q.5 Average amount spent on fruits
mean(campaign_new$MntFruits)

26.35605

# Q.10 Average age of customers
mean(campaign_new$Age)

52 

#####################################################################################################################
