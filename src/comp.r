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

# install.packages("psych")
library(psych)

# Load CSV data
full_data <- read.csv(file.choose())
View(full_data)
#import cleaned dataset with variables for project
clean_data <- read.csv(file.choose())
View(clean_data)

#filter out a subset of data with only numerical values
numeric <- select(clean_data, -(Country), -(Education), -(Marital_Status))

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

#individual correlations

#Age vs categorical purchases
cor(numeric$Year_Birth, numeric$MntWines, method ="pearson")
cor(numeric$Year_Birth, numeric$MntFruits, method ="pearson")
cor(numeric$Year_Birth, numeric$MntMeatProducts, method ="pearson")
cor(numeric$Year_Birth, numeric$MntFishProducts, method ="pearson")
cor(numeric$Year_Birth, numeric$MntSweetProducts, method ="pearson")

#Family Size vs categorical purchases


#Family Life Cycle vs categorical purchases


#Education vs categorical purchases


#Location vs categorical purchases


#Income vs categorical purchases
cor(numeric$Income, numeric$MntWines, method ="pearson")
cor(numeric$Income, numeric$MntFruits, method ="pearson")
cor(numeric$Income, numeric$MntMeatProducts, method ="pearson")
cor(numeric$Income, numeric$MntFishProducts, method ="pearson")
cor(numeric$Income, numeric$MntSweetProducts, method ="pearson")

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

#Individual Correlations

#Age vs discount purchase
cor(numeric$Year_Birth, numeric$NumDealsPurchases, method ="pearson")

#Family size vs discount purchase

#Marital Status vs discount purchase

#Education vs discount purchase

#Location vs discount purchase

#Income vs discount purchase
cor(numeric$Income, numeric$NumDealsPurchases, method ="pearson")

##Beginning Hypothesis 3

##Beginning Hypothesis 4
