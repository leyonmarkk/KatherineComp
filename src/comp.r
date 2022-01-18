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
#filter out variables being used for project
comp_data <- select(full_data, -(ID), -(Dt_Customer), -(Recency), -(MntGoldProds), -(NumWebVisitsMonth), -(AcceptedCmp3), -(AcceptedCmp4), -(AcceptedCmp5), -(AcceptedCmp1), -(AcceptedCmp2), -(Response), -(Complain))

#filter out a subset of data with only numerical values
numeric <- select(comp_data, -(Country), -(Education), -(Marital_Status))

#full correlation set via cor plot
corPlot(numeric)

## Beginning Hypothesis 1

#Graphing individual variables

#Age vs categorial purchases
ggplot(data=comp_data, aes(x=Year_Birth, y=MntWines)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

#Family Size vs categorical purchases


#Family Life Cycle vs categorical purchases
ggplot(data=comp_data, aes(x=Marital_Status, y=MntWines)) + geom_line( color="red") + ggtitle("Amount spent on wine in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#Education vs categorical purchases
ggplot(data=comp_data, aes(x=Education, y=MntWines)) + geom_line( color="red")+ ggtitle("Amount spent on wine in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#Location vs categorical purchases
ggplot(data=comp_data, aes(x=Country, y=MntWines)) + geom_line( color="red")+ ggtitle("Amount spent on wine in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

#Income vs categorical purchases
ggplot(data=comp_data, aes(x=Income, y=MntWines)) + geom_line( color="red")+  ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Income in USD")

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
