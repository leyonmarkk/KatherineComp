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
install.packages("mediation")
library(mediation)

# Load CSV data
full_data <- read.csv(file.choose())
View(full_data)

#import cleaned dataset with variables for project
clean_data <- read.csv(file.choose(), colClasses=c("KidBinary"="character", "IncBinary"="character", "MarBinary"="character", "AgeBinary"="character", "EdBinary"="character"))
View(clean_data)

#creating a combined family size column
family_size <- str_c(clean_data$Kidhome, '', clean_data$Teenhome)

## Beginning Hypothesis 1:

#Graphing individual variables to show basic correlations visually

#Age vs categorial purchases
#(a)
ggplot(data=clean_data, aes(x=Year_Birth, y=MntWines)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#(b)
ggplot(data=clean_data, aes(x=Year_Birth, y=MntFruits)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fruit in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#(c)
ggplot(data=clean_data, aes(x=Year_Birth, y=MntMeatProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on meat in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#(d)
ggplot(data=clean_data, aes(x=Year_Birth, y=MntFishProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fish in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#(e)
ggplot(data=clean_data, aes(x=Year_Birth, y=MntSweetProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on sweets in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")+ geom_vline(xintercept = 1946, color ="blue")+ geom_vline(xintercept = 1965, color ="blue")+ geom_vline(xintercept = 1977, color ="blue")+ geom_vline(xintercept = 1995, color ="blue")

#Family Size vs categorical purchases
#(a)
ggplot(data=clean_data, aes(x=Family_Size, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

#(b)
ggplot(data=clean_data, aes(x=Family_Size, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

#(c)
ggplot(data=clean_data, aes(x=Family_Size, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

#(d)
ggplot(data=clean_data, aes(x=Family_Size, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

#(e)
ggplot(data=clean_data, aes(x=Family_Size, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by number of children at home") + ylab("Value in USD")+ xlab("Number of Children at Home")

#Family Life Cycle vs categorical purchases
#(a)
ggplot(data=clean_data, aes(x=Marital_Status, y=MntWines)) + geom_point( color="red") + ggtitle("Amount spent on wine in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#(b)
ggplot(data=clean_data, aes(x=Marital_Status, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#(c)
ggplot(data=clean_data, aes(x=Marital_Status, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#(d)
ggplot(data=clean_data, aes(x=Marital_Status, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#(e)
ggplot(data=clean_data, aes(x=Marital_Status, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by marital status") + ylab("Value in USD")+ xlab("Marital Status")

#Education vs categorical purchases
#(a)
ggplot(data=clean_data, aes(x=Education, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#(b)
ggplot(data=clean_data, aes(x=Education, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#(c)
ggplot(data=clean_data, aes(x=Education, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#(d)
ggplot(data=clean_data, aes(x=Education, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#(e)
ggplot(data=clean_data, aes(x=Education, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by education level") + ylab("Value in USD")+ xlab("Education Level")

#Income vs categorical purchases
#(a)
ggplot(data=clean_data, aes(x=Income, y=MntWines)) + geom_line( color="red")+  ggtitle("Amount spent on wine in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45)) + xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#(b)
ggplot(data=clean_data, aes(x=Income, y=MntFruits)) + geom_line( color="red")+  ggtitle("Amount spent on fruit in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#(c)
ggplot(data=clean_data, aes(x=Income, y=MntMeatProducts)) + geom_line( color="red")+  ggtitle("Amount spent on meat in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#(d)
ggplot(data=clean_data, aes(x=Income, y=MntFishProducts)) + geom_line( color="red")+  ggtitle("Amount spent on fish in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#(e)
ggplot(data=clean_data, aes(x=Income, y=MntSweetProducts)) + geom_line( color="red")+  ggtitle("Amount spent on sweets in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)+ geom_vline(xintercept = 50000, color ="blue")+ geom_vline(xintercept = 100000, color ="blue")+ geom_vline(xintercept = 150000, color ="blue")+ geom_vline(xintercept = 200000, color ="blue")

#Individual correlations and regressions to show numerical relations, as well as graphical trends

#Age vs categorical purchases
#(a)
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
#(b)
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
#(c)
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
#(d)
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
#(e)
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
modOnein <- lm(MntWines ~ Income,data = clean_data)
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
cor(clean_data$MarScore, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$MarScore, clean_data$NumDealsPurchases)
# create the linear model
modOnemardis <- lm(MarScore ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOnemardis)
clean_data %>% ggplot(aes(x = MarScore, y = NumDealsPurchases)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Education vs discount purchase
cor(clean_data$EdScore, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$EdScore, clean_data$NumDealsPurchases)
# create the linear model
modOneeddis <- lm(EdScore ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOneeddis)
clean_data %>% ggplot(aes(x = EdScore, y = NumDealsPurchases)) +
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

#Persona 1:

#Step 1: Total persona score vs individual goods linear regressions
#(a)
fit.totaleffect1=lm(MntWines~Persona1, data=clean_data)
summary(fit.totaleffect1)

#(b)
fit.totaleffect2=lm(MntFruits~Persona1, data=clean_data)
summary(fit.totaleffect2)

#(c)
fit.totaleffect3=lm(MntMeatProducts~Persona1, data=clean_data)
summary(fit.totaleffect3)

#(d)
fit.totaleffect4=lm(MntFishProducts~Persona1, data=clean_data)
summary(fit.totaleffect4)

#(e)
fit.totaleffect5=lm(MntSweetProducts~Persona1, data=clean_data)
summary(fit.totaleffect5)

#Step 2: Linear regressions with mediator (channel method) and personas
#(a)
fit.mediator1=lm(NumWebPurchases~Persona1, data=clean_data)
summary(fit.mediator1)

#(b)
fit.mediator2=lm(NumCatalogPurchases~Persona1, data=clean_data)
summary(fit.mediator2)

#(c)
fit.mediator3=lm(NumStorePurchases~Persona1, data=clean_data)
summary(fit.mediator3)

#Step 3: Linear regressions between mediator (channel method) and individual goods
#(a)(a)
fit.dv1=lm(MntWines~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv1)

#(b)(a)
fit.dv2=lm(MntFruits~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv2)

#(c)(a)
fit.dv3=lm(MntMeatProducts~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv3)

#(d)(a)
fit.dv4=lm(MntFishProducts~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv4)

#(e)(a)
fit.dv5=lm(MntSweetProducts~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv5)

#(a)(b)
fit.dv6=lm(MntWines~Persona1+NumCatalogPurchases,data=clean_data)
summary(fit.dv6)

#(b)(b)
fit.dv7=lm(MntFruits~Persona1+NumCatalogPurchases,data=clean_data)
summary(fit.dv7)

#(c)(b)
fit.dv8=lm(MntMeatProducts~Persona1+NumCatalogPurchases,data=clean_data)
summary(fit.dv8)

#(d)(b)
fit.dv9=lm(MntFishProducts~Persona1+NumCatalogPurchases,data=clean_data)
summary(fit.dv9)

#(e)(b)
fit.dv10=lm(MntSweetProducts~Persona1+CatalogPurchases,data=clean_data)
summary(fit.dv10)

#(a)(c)
fit.dv11=lm(MntWines~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv11)

#(b)(c)
fit.dv12=lm(MntFruits~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv12)

#(c)(c)
fit.dv13=lm(MntMeatProducts~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv13)

#(d)(c)
fit.dv14=lm(MntFishProducts~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv14)

#(e)(c)
fit.dv15=lm(MntSweetProducts~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv15)

#Step 4: mediation analyses
#(a)(a)
results1 = mediate(fit.mediator1, fit.dv1, treat='Persona1', mediator='NumWebPurchases', boot=T)

#(b)(a)
results2 = mediate(fit.mediator1, fit.dv2, treat='Persona1', mediator='NumWebPurchases', boot=T)

#(c)(a)
results3 = mediate(fit.mediator1, fit.dv3, treat='Persona1', mediator='NumWebPurhcases', boot=T)

#(d)(a)
results4 = mediate(fit.mediator1, fit.dv4, treat='Persona1', mediator='NumWebPurchases', boot=T)

#(e)(a)
results5 = mediate(fit.mediator1, fit.dv5, treat='Persona1', mediator='NumWebPurchases', boot=T)

#(a)(b)
results6 = mediate(fit.mediator2, fit.dv6, treat='Persona1', mediator='NumCatalogPurchases', boot=T)

#(b)(b)
results7 = mediate(fit.mediator2, fit.dv7, treat='Persona1', mediator='NumCatalogPurchases', boot=T)

#(c)(b)
results8 = mediate(fit.mediator2, fit.dv8, treat='Persona1', mediator='NumCatalogPurchases', boot=T)

#(d)(b)
results9 = mediate(fit.mediator2, fit.dv9, treat='Persona1', mediator='NumCatalogPurchases', boot=T)

#(e)(b)
results10 = mediate(fit.mediator2, fit.dv10, treat='Persona1', mediator ='NumCatalogPurchases', boot=T)

#(a)(c)
results11 = mediate(fit.mediator3, fit.dv11, treat='Persona1', mediator='NumStorePurchases', boot=T)

#(b)(c)
results12 = mediate(fit.mediator3, fit.dv12, treat='Persona1', mediator='NumStorePurchases', boot=T)

#(c)(c)
results13 = mediate(fit.mediator3, fit.dv13, treat='Persona1', mediator='NumStorePurchases', boot=T)

#(d)(c)
results14 = mediate(fit.mediator3, fit.dv14, treat='Persona1', mediator='NumStorePurchases', boot=T)

#(e)(c)
results15 = mediate(fit.mediator3, fit.dv15, treat='Persona1', mediator='NumStorePurchases', boot=T)

#Step 5: results

summary(results1)
summary(results2)
summary(results3)
summary(results4)
summary(results5)
summary(results6)
summary(results7)
summary(results8)
summary(results9)
summary(results10)
summary(results11)
summary(results12)
summary(results13)
summary(results14)
summary(results15)

#Persona 2:

#Step 1: Total persona score vs individual goods linear regressions
#(a)
fit.totaleffect1=lm(MntWines~Persona2, data=clean_data)
summary(fit.totaleffect1)

#(b)
fit.totaleffect2=lm(MntFruits~Persona2, data=clean_data)
summary(fit.totaleffect2)

#(c)
fit.totaleffect3=lm(MntMeatProducts~Persona2, data=clean_data)
summary(fit.totaleffect3)

#(d)
fit.totaleffect4=lm(MntFishProducts~Persona2, data=clean_data)
summary(fit.totaleffect4)

#(e)
fit.totaleffect5=lm(MntSweetProducts~Persona2, data=clean_data)
summary(fit.totaleffect5)

#Step 2: Linear regressions with mediator (channel method) and personas
#(a)
fit.mediator1=lm(NumWebPurchases~Persona2, data=clean_data)
summary(fit.mediator1)

#(b)
fit.mediator2=lm(NumCatalogPurchases~Persona2, data=clean_data)
summary(fit.mediator2)

#(c)
fit.mediator3=lm(NumStorePurchases~Persona2, data=clean_data)
summary(fit.mediator3)

#Step 3: Linear regressions between mediator (channel method) and individual goods
#(a)(a)
fit.dv1=lm(MntWines~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv1)

#(b)(a)
fit.dv2=lm(MntFruits~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv2)

#(c)(a)
fit.dv3=lm(MntMeatProducts~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv3)

#(d)(a)
fit.dv4=lm(MntFishProducts~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv4)

#(e)(a)
fit.dv5=lm(MntSweetProducts~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv5)

#(a)(b)
fit.dv6=lm(MntWines~Persona2+NumCatalogPurchases,data=clean_data)
summary(fit.dv6)

#(b)(b)
fit.dv7=lm(MntFruits~Persona2+NumCatalogPurchases,data=clean_data)
summary(fit.dv7)

#(c)(b)
fit.dv8=lm(MntMeatProducts~Persona2+NumCatalogPurchases,data=clean_data)
summary(fit.dv8)

#(d)(b)
fit.dv9=lm(MntFishProducts~Persona2+NumCatalogPurchases,data=clean_data)
summary(fit.dv9)

#(e)(b)
fit.dv10=lm(MntSweetProducts~Persona2+CatalogPurchases,data=clean_data)
summary(fit.dv10)

#(a)(c)
fit.dv11=lm(MntWines~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv11)

#(b)(c)
fit.dv12=lm(MntFruits~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv12)

#(c)(c)
fit.dv13=lm(MntMeatProducts~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv13)

#(d)(c)
fit.dv14=lm(MntFishProducts~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv14)

#(e)(c)
fit.dv15=lm(MntSweetProducts~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv15)

#Step 4: mediation analyses
#(a)(a)
results1 = mediate(fit.mediator1, fit.dv1, treat='Persona2', mediator='NumWebPurchases', boot=T)

#(b)(a)
results2 = mediate(fit.mediator1, fit.dv2, treat='Persona2', mediator='NumWebPurchases', boot=T)

#(c)(a)
results3 = mediate(fit.mediator1, fit.dv3, treat='Persona2', mediator='NumWebPurhcases', boot=T)

#(d)(a)
results4 = mediate(fit.mediator1, fit.dv4, treat='Persona2', mediator='NumWebPurchases', boot=T)

#(e)(a)
results5 = mediate(fit.mediator1, fit.dv5, treat='Persona2', mediator='NumWebPurchases', boot=T)

#(a)(b)
results6 = mediate(fit.mediator2, fit.dv6, treat='Persona2', mediator='NumCatalogPurchases', boot=T)

#(b)(b)
results7 = mediate(fit.mediator2, fit.dv7, treat='Persona2', mediator='NumCatalogPurchases', boot=T)

#(c)(b)
results8 = mediate(fit.mediator2, fit.dv8, treat='Persona2', mediator='NumCatalogPurchases', boot=T)

#(d)(b)
results9 = mediate(fit.mediator2, fit.dv9, treat='Persona2', mediator='NumCatalogPurchases', boot=T)

#(e)(b)
results10 = mediate(fit.mediator2, fit.dv10, treat='Persona2', mediator ='NumCatalogPurchases', boot=T)

#(a)(c)
results11 = mediate(fit.mediator3, fit.dv11, treat='Persona2', mediator='NumStorePurchases', boot=T)

#(b)(c)
results12 = mediate(fit.mediator3, fit.dv12, treat='Persona2', mediator='NumStorePurchases', boot=T)

#(c)(c)
results13 = mediate(fit.mediator3, fit.dv13, treat='Persona2', mediator='NumStorePurchases', boot=T)

#(d)(c)
results14 = mediate(fit.mediator3, fit.dv14, treat='Persona2', mediator='NumStorePurchases', boot=T)

#(e)(c)
results15 = mediate(fit.mediator3, fit.dv15, treat='Persona2', mediator='NumStorePurchases', boot=T)

#Step 5: results

summary(results1)
summary(results2)
summary(results3)
summary(results4)
summary(results5)
summary(results6)
summary(results7)
summary(results8)
summary(results9)
summary(results10)
summary(results11)
summary(results12)
summary(results13)
summary(results14)
summary(results15)

##Beginning Hypothesis 4

#Persona 1:

#Step 1: Total persona score vs individual goods linear regressions
fit.totaleffect1=lm(NumDealsPurchases~Persona1, data=clean_data)
summary(fit.totaleffect1)

#Step 2: Linear regressions with mediator (channel method) and personas
#(a)
fit.mediator1=lm(NumWebPurchases~Persona1, data=clean_data)
summary(fit.mediator1)

#(b)
fit.mediator2=lm(NumCatalogPurchases~Persona1, data=clean_data)
summary(fit.mediator2)

#(c)
fit.mediator3=lm(NumStorePurchases~Persona1, data=clean_data)
summary(fit.mediator3)

#Step 3: Linear regressions between mediator (channel method) and individual goods
#(a)
fit.dv1=lm(NumDealsPurchases~Persona1+NumWebPurchases,data=clean_data)
summary(fit.dv1)

#(b)
fit.dv2=lm(NumDealsPurchases~Persona1+NumCatalogPurchases,data=clean_data)
summary(fit.dv2)

#(c)
fit.dv3=lm(NunDealsPurchases~Persona1+NumStorePurchases,data=clean_data)
summary(fit.dv3)

#Step 4: mediation analyses
#(a)
results1 = mediate(fit.mediator1, fit.dv1, treat='Persona1', mediator='NumWebPurchases', boot=T)

#(b)
results2 = mediate(fit.mediator2, fit.dv2, treat='Persona1', mediator='NumCatalogPurchases', boot=T)

#(c)
results3 = mediate(fit.mediator3, fit.dv3, treat='Persona1', mediator='NumStorePurhcases', boot=T)

#Step 5: results

summary(results1)
summary(results2)
summary(results3)

#Persona 2:

#Step 1: Total persona score vs individual goods linear regressions
fit.totaleffect1=lm(NumDealsPurchases~Persona2, data=clean_data)
summary(fit.totaleffect1)

#Step 2: Linear regressions with mediator (channel method) and personas
#(a)
fit.mediator1=lm(NumWebPurchases~Persona2, data=clean_data)
summary(fit.mediator1)

#(b)
fit.mediator2=lm(NumCatalogPurchases~Persona2, data=clean_data)
summary(fit.mediator2)

#(c)
fit.mediator3=lm(NumStorePurchases~Persona2, data=clean_data)
summary(fit.mediator3)

#Step 3: Linear regressions between mediator (channel method) and individual goods
#(a)
fit.dv1=lm(NumDealsPurchases~Persona2+NumWebPurchases,data=clean_data)
summary(fit.dv1)

#(b)
fit.dv2=lm(NumDealsPurchases~Persona2+NumCatalogPurchases,data=clean_data)
summary(fit.dv2)

#(c)
fit.dv3=lm(NunDealsPurchases~Persona2+NumStorePurchases,data=clean_data)
summary(fit.dv3)

#Step 4: mediation analyses
#(a)
results1 = mediate(fit.mediator1, fit.dv1, treat='Persona2', mediator='NumWebPurchases', boot=T)

#(b)
results2 = mediate(fit.mediator2, fit.dv2, treat='Persona2', mediator='NumCatalogPurchases', boot=T)

#(c)
results3 = mediate(fit.mediator3, fit.dv3, treat='Persona2', mediator='NumStorePurhcases', boot=T)

#Step 5: results

summary(results1)
summary(results2)
summary(results3)
