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

## Beginning Hypothesis 1:

#Graphing individual variables to show basic correlations visually

#Age vs categorial purchases
ggplot(data=clean_data, aes(x=Year_Birth, y=MntWines)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFruits)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fruit in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntMeatProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on meat in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFishProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fish in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntSweetProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on sweets in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#Family Size vs categorical purchases
ggplot(data=clean_data, aes(x=Family_Size, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

ggplot(data=clean_data, aes(x=Family_Size, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

ggplot(data=clean_data, aes(x=Family_Size, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

ggplot(data=clean_data, aes(x=Family_Size, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

ggplot(data=clean_data, aes(x=Family_Size, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

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

#Income vs categorical purchases
ggplot(data=clean_data, aes(x=Income, y=MntWines)) + geom_line( color="red")+  ggtitle("Amount spent on wine in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45)) + xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

ggplot(data=clean_data, aes(x=Income, y=MntFruits)) + geom_line( color="red")+  ggtitle("Amount spent on fruit in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

ggplot(data=clean_data, aes(x=Income, y=MntMeatProducts)) + geom_line( color="red")+  ggtitle("Amount spent on meat in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

ggplot(data=clean_data, aes(x=Income, y=MntFishProducts)) + geom_line( color="red")+  ggtitle("Amount spent on fish in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

ggplot(data=clean_data, aes(x=Income, y=MntSweetProducts)) + geom_line( color="red")+  ggtitle("Amount spent on sweets in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#Individual correlations and regressions to show numerical relations, as well as graphical trends

#Age vs categorical purchases
cor(clean_data$Year_Birth, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$MntWines)
cor(clean_data$Year_Birth, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$MntFruits)
cor(clean_data$Year_Birth, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$MntMeatProducts)
cor(clean_data$Year_Birth, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$MntFishProducts)
cor(clean_data$Year_Birth, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$MntSweetProducts)

# create the linear models
modOneage <- lm(MntWines ~ Year_Birth,data = clean_data)
modTwoage <- lm(MntFruits ~ Year_Birth,data = clean_data)
modThreeage <- lm(MntMeatProducts ~ Year_Birth,data = clean_data)
modFourage <- lm(MntFishProducts ~ Year_Birth,data = clean_data)
modFiveage <- lm(MntSweetProducts~ Year_Birth,data = clean_data)

# run the linear model
summary(modOneage)
summary(modTwoage)
summary(modThreeage)
summary(modFourage)
summary(modFiveage)

clean_data %>% ggplot(aes(x = Year_Birth, y = MntWines)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Year_Birth, y = MntFruits)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Year_Birth, y = MntMeatProducts )) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Year_Birth, y = MntFishProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Year_Birth, y = MntSweetProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Family Size vs categorical purchases
cor(clean_data$Family_Size, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$MntWines)
cor(clean_data$Family_Size, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$MntFruits)
cor(clean_data$Family_Size, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$MntMeatProducts)
cor(clean_data$Family_Size, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$MntFishProducts)
cor(clean_data$Family_Size, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$MntSweetProducts)

# create the linear models
modOnefam <- lm(MntWines ~ Family_Size,data = clean_data)
modTwofam <- lm(MntFruits ~ Family_Size,data = clean_data)
modThreefam <- lm(MntMeatProducts ~ Family_Size,data = clean_data)
modFourfam <- lm(MntFishProducts ~ Family_Size,data = clean_data)
modFivefam <- lm(MntSweetProducts ~ Family_Size,data = clean_data)

# run the linear model
summary(modOnefam)
summary(modTwofam)
summary(modThreefam)
summary(modFourfam)
summary(modFivefam)

clean_data %>% ggplot(aes(x = Family_Size, y = MntWines)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Family_Size, y = MntFruits)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Family_Size, y = MntMeatProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Family_Size, y = MntFishProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = Family_Size, y = MntSweetProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Family Life Cycle vs categorical purchases
cor(clean_data$MarScore, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$MntWines)
cor(clean_data$MarScore, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$MntFruits)
cor(clean_data$MarScore, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$MntMeatProducts)
cor(clean_data$MarScore, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$MntFishProducts)
cor(clean_data$MarScore, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$MntSweetProducts)

# create the linear models
modOnemar <- lm(MntWines ~ MarScore,data = clean_data)
modTwomar <- lm(MntFruits ~ MarScore,data = clean_data)
modThreemar <- lm(MntMeatProducts ~ MarScore,data = clean_data)
modFourmar <- lm(MntFishProducts ~ MarScore,data = clean_data)
modFivemar <- lm(MntSweetProducts ~ MarScore,data = clean_data)

# run the linear model
summary(modOnemar)
summary(modTwomar)
summary(modThreemar)
summary(modFourmar)
summary(modFivemar)

clean_data %>% ggplot(aes(x = MarScore, y = MntWines)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MarScore, y = MntFruits)) + geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MarScore, y = MntMeatProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MarScore, y = MntFishProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MarScore, y = MntSweetProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Education vs categorical purchases
cor(clean_data$EdScore, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$MntWines)
cor(clean_data$EdScore, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$MntFruits)
cor(clean_data$EdScore, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$MntMeatProducts)
cor(clean_data$EdScore, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$MntFishProducts)
cor(clean_data$EdScore, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$MntSweetProducts)

# create the linear models
modOneed <- lm(MntWines ~ EdScore,data = clean_data)
modTwoed <- lm(MntFruits ~ EdScore,data = clean_data)
modThreeed <- lm(MntMeatProducts ~ EdScore,data = clean_data)
modFoured <- lm(MntFishProducts ~ EdScore,data = clean_data)
modFiveed <- lm(MntSweetProducts ~ EdScore,data = clean_data)

# run the linear model
summary(modOneed)
summary(modTwoed)
summary(modThreeed)
summary(modFoured)
summary(modFiveed)

clean_data %>% ggplot(aes(x = EdScore, y = MntWines)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = EdScore, y = MntFruits)) +
                        geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = EdScore, y = MntMeatProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = EdScore, y = MntFishProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = EdScore, y = MntSweetProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Income vs categorical purchases
cor(clean_data$Income, clean_data$MntWines, method ="pearson", use = "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$MntWines)
cor(clean_data$Income, clean_data$MntFruits, method ="pearson", use = "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$MntFruits)
cor(clean_data$Income, clean_data$MntMeatProducts, method ="pearson", use = "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$MntMeatProducts)
cor(clean_data$Income, clean_data$MntFishProducts, method ="pearson", use = "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$MntFishProducts)
cor(clean_data$Income, clean_data$MntSweetProducts, method ="pearson", use = "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$MntSweetProducts)

# create the linear models
modOnein <- lm(Mntwines ~ Income,data = clean_data)
modTwoin <- lm(MntFruits ~ Income,data = clean_data)
modThreein <- lm(MntMeatProducts ~ Income,data = clean_data)
modFourin <- lm(MntFishProducts ~ Income,data = clean_data)
modFivein <- lm(MntSweetProducts ~ Income,data = clean_data)

# run the linear model
summary(modOnein)
summary(modTwoin)
summary(modThreein)
summary(modFourin)
summary(modFivein)

clean_data %>% ggplot(aes(x = Income, y = MntWines)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) +
  theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

clean_data %>% ggplot(aes(x = Income, y = MntFruits)) + geom_point(alpha = I(1/4))+ geom_smooth(method = lm)+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

clean_data %>% ggplot(aes(x = Income, y = MntMeatProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

clean_data %>% ggplot(aes(x = Income, y = MntFishProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

clean_data %>% ggplot(aes(x = Income, y = MntSweetProducts)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

##Beginning Hypothesis 2:

#Graphing individual variables to start, to show basic correlations visually

#Age vs discount purchase
ggplot(data=clean_data, aes(x=Year_Birth, y=NumDealsPurchases)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Number of purchases made with a discount by age")+ xlab("Birth Year (YYYY)") + geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#Family size vs discount purchase
ggplot(data=clean_data, aes(x=Family_Size, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by number of children at home")+ xlab("Number of Children at Home")

#Marital Status vs discount purchase
ggplot(data=clean_data, aes(x=Marital_Status, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by marital status")+ xlab("Marital Status")

#Education vs discount purchase
ggplot(data=clean_data, aes(x=Education, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by education level")+ xlab("Education Level")

#Income vs discount purchase
ggplot(data=clean_data, aes(x= Income, y=NumDealsPurchases)) + geom_line( color="red")+ ggtitle("Number of purchases made with a discount by income")+ xlab("Yearly Household Income") + theme(axis.text.x = element_text(angle = 45)) +xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#Individual Correlations and regressions to show numerical relations, as well as graphical trends

#Age vs discount purchase
cor(clean_data$Year_Birth, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$NumDealsPurchases)
# create the linear model
modOneagedis <- lm(Year_Birth ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOneagedis)
clean_data %>% ggplot(aes(x = Year_Birth, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Family size vs discount purchase
cor(clean_data$Family_Size, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$NumDealsPurchases)
# create the linear model
modOnefamdis <- lm(Family_Size ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOnefamdis)
clean_data %>% ggplot(aes(x = Family_Size, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Marital Status vs discount purchase
cor(clean_data$Marital_Status, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$NumDealsPurchases)
# create the linear model
modOnemardis <- lm(Marital_Status ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOnemardis)
clean_data %>% ggplot(aes(x = Marital_Status, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Education vs discount purchase
cor(clean_data$Education, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$NumDealsPurchases)
# create the linear model
modOneeddis <- lm(Education ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOneeddis)
clean_data %>% ggplot(aes(x = Education, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Income vs discount purchase
cor(clean_data$Income, clean_data$NumDealsPurchases, method ="pearson", use= "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$NumDealsPurchases)
# create the linear model
modOneincdis <- lm(Income ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOneincdis)
clean_data %>% ggplot(aes(x = Income, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) +xlim(0, 200000)

##Beginning Hypothesis 3

##Beginning Hypothesis 4
