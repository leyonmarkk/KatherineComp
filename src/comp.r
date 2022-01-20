### Name:
# Katherine Leyonmark
# Spring 2022
# Senior Comprehensive Project Code

rm(list = ls()) # remove variables stored in memory.
graphics.off() # clear out all plots from previous work.
cat("\014") # clear the console

# adding libraries here
library(tidyverse)
library(ggplot2)
library(stringr)
library(psych)
library(plyr)

# Load CSV data
full_data <- read.csv(file.choose())
View(full_data)
#import cleaned dataset with variables for project
clean_data <- read.csv(file.choose())
View(clean_data)

#creating a combined family size column
family_size <- str_c(clean_data$Kidhome, '', clean_data$Teenhome)

#filter out a subset of data with only numerical values
numeric <- select(clean_data, -(Country), -(Education), -(Marital_Status))

#assigning numeric values to character strings
clean_data$Education<- revalue(clean_data$Education, c("High School"="1", "Bachelor"="2", "Master"="3", "PhD"="4"))

#full correlation set via cor plot
corPlot(numeric)

## Beginning Hypothesis 1

#Graphing individual variables

#Age vs categorial purchases
ggplot(data=clean_data, aes(x=Year_Birth, y=MntWines)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFruits)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fruit in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntMeatProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on meat in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFishProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fish in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntSweetProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on sweets in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

#Family Size vs categorical purchases


#Family Life Cycle vs categorical purchases
ggplot(data=clean_data, aes(x=Marital_Status, y=MntWines)) + geom_point( color="red") + ggtitle("Amount spent on wine in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

ggplot(data=clean_data, aes(x=Marital_Status, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

ggplot(data=clean_data, aes(x=Marital_Status, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

ggplot(data=clean_data, aes(x=Marital_Status, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

ggplot(data=clean_data, aes(x=Marital_Status, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#Education vs categorical purchases
ggplot(data=clean_data, aes(x=Education, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

ggplot(data=clean_data, aes(x=Education, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

ggplot(data=clean_data, aes(x=Education, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

ggplot(data=clean_data, aes(x=Education, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

ggplot(data=clean_data, aes(x=Education, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#Location vs categorical purchases
ggplot(data=clean_data, aes(x=Country, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

#Income vs categorical purchases
ggplot(data=clean_data, aes(x=Income, y=MntWines)) + geom_line( color="red")+  ggtitle("Amount spent on wine in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")

ggplot(data=clean_data, aes(x=Income, y=MntFruits)) + geom_line( color="red")+  ggtitle("Amount spent on fruit in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")

ggplot(data=clean_data, aes(x=Income, y=MntMeatProducts)) + geom_line( color="red")+  ggtitle("Amount spent on meat in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")

ggplot(data=clean_data, aes(x=Income, y=MntFishProducts)) + geom_line( color="red")+  ggtitle("Amount spent on fish in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")

ggplot(data=clean_data, aes(x=Income, y=MntSweetProducts)) + geom_line( color="red")+  ggtitle("Amount spent on sweets in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")

#individual correlations and regressions

#Age vs categorical purchases
cor(numeric$Year_Birth, numeric$MntWines, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$MntWines)
cor(numeric$Year_Birth, numeric$MntFruits, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$MntFruits)
cor(numeric$Year_Birth, numeric$MntMeatProducts, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$MntMeatProducts)
cor(numeric$Year_Birth, numeric$MntFishProducts, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$MntFishProducts)
cor(numeric$Year_Birth, numeric$MntSweetProducts, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$MntSweetProducts)

# create the linear models
modOne <- lm(Year_Birth ~ MntWines,data = numeric)
modTwo <- lm(Year_Birth ~ MntFruits,data = numeric)
modThree <- lm(Year_Birth ~ MntMeatProducts,data = numeric)
modFour <- lm(Year_Birth ~ MntFishProducts,data = numeric)
modFive <- lm(Year_Birth ~ MntSweetProducts,data = numeric)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

numeric %>% ggplot(aes(x = MntWines, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

numeric %>% ggplot(aes(x = MntFruits, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

numeric %>% ggplot(aes(x = MntMeatProducts, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

numeric %>% ggplot(aes(x = MntFishProducts, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

numeric %>% ggplot(aes(x = MntSweetProducts, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

#Family Size vs categorical purchases


#Family Life Cycle vs categorical purchases


#Education vs categorical purchases


#Location vs categorical purchases


#Income vs categorical purchases
cor(numeric$Income, numeric$MntWines, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$MntWines)
cor(numeric$Income, numeric$MntFruits, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$MntFruits)
cor(numeric$Income, numeric$MntMeatProducts, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$MntMeatProducts)
cor(numeric$Income, numeric$MntFishProducts, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$MntFishProducts)
cor(numeric$Income, numeric$MntSweetProducts, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$MntSweetProducts)

# create the linear models
modOne <- lm(Income ~ MntWines,data = numeric)
modTwo <- lm(Income ~ MntFruits,data = numeric)
modThree <- lm(Income ~ MntMeatProducts,data = numeric)
modFour <- lm(Income ~ MntFishProducts,data = numeric)
modFive <- lm(Income ~ MntSweetProducts,data = numeric)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

numeric %>% ggplot(aes(x = MntWines, y = Income)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

numeric %>% ggplot(aes(x = MntFruits, y = Income) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm))

numeric %>% ggplot(aes(x = MntMeatProducts, y = Income)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

numeric %>% ggplot(aes(x = MntFishProducts, y = Income)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

numeric %>% ggplot(aes(x = MntSweetProducts, y = Income)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

##Beginning Hypothesis 2

#Graphing individual variables

#Age vs discount purchase
ggplot(data=clean_data, aes(x=Year_Birth, y=NumDealsPurchases)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Number of purchases made with a discount by age")+ xlab("Birth Year (YYYY)")

#Family size vs discount purchase


#Marital Status vs discount purchase
ggplot(data=clean_data, aes(x=Marital_Status, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by marital status")+ xlab("Marital Status")

#Education vs discount purchase
ggplot(data=clean_data, aes(x=Education, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by education level")+ xlab("Education Level")

#Location vs discount purchase
ggplot(data=clean_data, aes(x=Country, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by country")+ xlab("Country")

#Income vs discount purchase
ggplot(data=clean_data, aes(x= Income, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by income")+ xlab("Yearly Household Income")

#Individual Correlations and regressions

#Age vs discount purchase
cor(numeric$Year_Birth, numeric$NumDealsPurchases, method ="pearson")
t.test(data = numeric, numeric$Year_Birth, numeric$NumDealsPurchases)
# create the linear model
modOne <- lm(Year_Birth ~ NumDealsPurchases,data = numeric)
# run the linear model
summary(modOne)
numeric %>% ggplot(aes(x = NumDealsPurchases, y = Year_Birth)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

#Family size vs discount purchase

#Marital Status vs discount purchase

#Education vs discount purchase

#Location vs discount purchase

#Income vs discount purchase
cor(numeric$Income, numeric$NumDealsPurchases, method ="pearson")
t.test(data = numeric, numeric$Income, numeric$NumDealsPurchases)
# create the linear model
modOne <- lm(Income ~ NumDealsPurchases,data = numeric)
# run the linear model
summary(modOne)
numeric %>% ggplot(aes(x = NumDealsPurchases, y = Income)) + 
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) 

##Beginning Hypothesis 3

##Beginning Hypothesis 4
