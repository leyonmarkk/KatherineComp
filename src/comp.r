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

#assigning numeric values to character strings
clean_data$Education <- revalue(clean_data$Education, c("High School"="1", "Bachelor"="2", "Master"="3", "PhD"="4"))

clean_data$Marital_Status <- revalue(clean_data$Education, c("Single"="1", "Together"="2", "Married"="3", "Divorced"="4", "Widow"="5"))

clean_data$Country <- revalue(clean_data$Education, c("AUS"="1", "CA"="2", "GER"="3", "IND"="4", "SA"= "5", "SP"="6", "US"="7"))

#converting char strings to dbl
clean_data$Education <- as.numeric(as.character(clean_data$Education))
clean_data$Marital_Status <- as.numeric(as.character(clean_data$Marital_Status))
clean_data$Country <- as.numeric(as.character(clean_data$Country))

#full correlation set via cor plot
corPlot(clean_data)

## Beginning Hypothesis 1

#Graphing individual variables

#Age vs categorial purchases
ggplot(data=clean_data, aes(x=Year_Birth, y=MntWines)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on wine in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFruits)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fruit in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntMeatProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on meat in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntFishProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on fish in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

ggplot(data=clean_data, aes(x=Year_Birth, y=MntSweetProducts)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Amount spent on sweets in the past 2 years by age") + ylab("Value in USD")+ xlab("Birth Year (YYYY)")

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

#Location vs categorical purchases
ggplot(data=clean_data, aes(x=Country, y=MntWines)) + geom_point( color="red")+ ggtitle("Amount spent on wine in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntFruits)) + geom_point( color="red")+ ggtitle("Amount spent on fruit in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntMeatProducts)) + geom_point( color="red")+ ggtitle("Amount spent on meat in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntFishProducts)) + geom_point( color="red")+ ggtitle("Amount spent on fish in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

ggplot(data=clean_data, aes(x=Country, y=MntSweetProducts)) + geom_point( color="red")+ ggtitle("Amount spent on sweets in the past 2 years by country") + ylab("Value in USD")+ xlab("Country")

#Income vs categorical purchases
ggplot(data=clean_data, aes(x=Income, y=MntWines)) + geom_line( color="red")+  ggtitle("Amount spent on wine in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45)) + xlim(0, 200000)

ggplot(data=clean_data, aes(x=Income, y=MntFruits)) + geom_line( color="red")+  ggtitle("Amount spent on fruit in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

ggplot(data=clean_data, aes(x=Income, y=MntMeatProducts)) + geom_line( color="red")+  ggtitle("Amount spent on meat in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

ggplot(data=clean_data, aes(x=Income, y=MntFishProducts)) + geom_line( color="red")+  ggtitle("Amount spent on fish in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD")+ theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

ggplot(data=clean_data, aes(x=Income, y=MntSweetProducts)) + geom_line( color="red")+  ggtitle("Amount spent on sweets in the past 2 years by income") + ylab("Value in USD")+ xlab("Income in USD") + theme(axis.text.x = element_text(angle = 45))+ xlim(0, 200000)

#individual correlations and regressions

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
modOne <- lm(Year_Birth ~ MntWines,data = clean_data)
modTwo <- lm(Year_Birth ~ MntFruits,data = clean_data)
modThree <- lm(Year_Birth ~ MntMeatProducts,data = clean_data)
modFour <- lm(Year_Birth ~ MntFishProducts,data = clean_data)
modFive <- lm(Year_Birth ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Year_Birth)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFruits, y = Year_Birth)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Year_Birth)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFishProducts, y = Year_Birth)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Year_Birth)) +
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
modOne <- lm(Family_Size ~ MntWines,data = clean_data)
modTwo <- lm(Family_Size ~ MntFruits,data = clean_data)
modThree <- lm(Family_Size ~ MntMeatProducts,data = clean_data)
modFour <- lm(Family_Size ~ MntFishProducts,data = clean_data)
modFive <- lm(Family_Size ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFruits, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFishProducts, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Family Life Cycle vs categorical purchases
cor(clean_data$Marital_Status, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$MntWines)
cor(clean_data$Marital_Status, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$MntFruits)
cor(clean_data$Marital_Status, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$MntMeatProducts)
cor(clean_data$Marital_Status, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$MntFishProducts)
cor(clean_data$Marital_Status, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$MntSweetProducts)

# create the linear models
modOne <- lm(Marital_Status ~ MntWines,data = clean_data)
modTwo <- lm(Marital_Status ~ MntFruits,data = clean_data)
modThree <- lm(Marital_Status ~ MntMeatProducts,data = clean_data)
modFour <- lm(Marital_Status ~ MntFishProducts,data = clean_data)
modFive <- lm(Marital_Status ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Marital_Status)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFruits, y = Marital_Status)) + geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Marital_Status)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFishProducts, y = Marital_Status)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Marital_Status)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Education vs categorical purchases
cor(clean_data$Education, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$MntWines)
cor(clean_data$Education, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$MntFruits)
cor(clean_data$Education, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$MntMeatProducts)
cor(clean_data$Education, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$MntFishProducts)
cor(clean_data$Education, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$MntSweetProducts)

# create the linear models
modOne <- lm(Education ~ MntWines,data = clean_data)
modTwo <- lm(Education ~ MntFruits,data = clean_data)
modThree <- lm(Education ~ MntMeatProducts,data = clean_data)
modFour <- lm(Education ~ MntFishProducts,data = clean_data)
modFive <- lm(Education ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Education)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFruits, y = Education)) +
                        geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Education)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFishProducts, y = Education)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Education)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)


#Location vs categorical purchases
cor(clean_data$Country, clean_data$MntWines, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$MntWines)
cor(clean_data$Country, clean_data$MntFruits, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$MntFruits)
cor(clean_data$Country, clean_data$MntMeatProducts, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$MntMeatProducts)
cor(clean_data$Country, clean_data$MntFishProducts, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$MntFishProducts)
cor(clean_data$Country, clean_data$MntSweetProducts, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$MntSweetProducts)

# create the linear models
modOne <- lm(Country ~ MntWines,data = clean_data)
modTwo <- lm(Country ~ MntFruits,data = clean_data)
modThree <- lm(Country ~ MntMeatProducts,data = clean_data)
modFour <- lm(Country ~ MntFishProducts,data = clean_data)
modFive <- lm(Country ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Country)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFruits, y = Country)) +
                        geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Country)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntFishProducts, y = Country)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Country)) +
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
modOne <- lm(Income ~ MntWines,data = clean_data)
modTwo <- lm(Income ~ MntFruits,data = clean_data)
modThree <- lm(Income ~ MntMeatProducts,data = clean_data)
modFour <- lm(Income ~ MntFishProducts,data = clean_data)
modFive <- lm(Income ~ MntSweetProducts,data = clean_data)

# run the linear model
summary(modOne)
summary(modTwo)
summary(modThree)
summary(modFour)
summary(modFive)

clean_data %>% ggplot(aes(x = MntWines, y = Income)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) +
  theme(axis.text.x = element_text(angle = 45))

clean_data %>% ggplot(aes(x = MntFruits, y = Income)) + geom_point(alpha = I(1/4))+ geom_smooth(method = lm)+ theme(axis.text.x = element_text(angle = 45))

clean_data %>% ggplot(aes(x = MntMeatProducts, y = Income)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))

clean_data %>% ggplot(aes(x = MntFishProducts, y = Income)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))

clean_data %>% ggplot(aes(x = MntSweetProducts, y = Income)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)+
  theme(axis.text.x = element_text(angle = 45))

##Beginning Hypothesis 2

#Graphing individual variables

#Age vs discount purchase
ggplot(data=clean_data, aes(x=Year_Birth, y=NumDealsPurchases)) + geom_line( color="red")+ scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + ggtitle("Number of purchases made with a discount by age")+ xlab("Birth Year (YYYY)") 

#Family size vs discount purchase
ggplot(data=clean_data, aes(x=Family_Size, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by number of children at home")+ xlab("Number of Children at Home")

#Marital Status vs discount purchase
ggplot(data=clean_data, aes(x=Marital_Status, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by marital status")+ xlab("Marital Status")

#Education vs discount purchase
ggplot(data=clean_data, aes(x=Education, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by education level")+ xlab("Education Level")

#Location vs discount purchase
ggplot(data=clean_data, aes(x=Country, y=NumDealsPurchases)) + geom_point( color="red")+ ggtitle("Number of purchases made with a discount by country")+ xlab("Country")

#Income vs discount purchase
ggplot(data=clean_data, aes(x= Income, y=NumDealsPurchases)) + geom_line( color="red")+ ggtitle("Number of purchases made with a discount by income")+ xlab("Yearly Household Income") + theme(axis.text.x = element_text(angle = 45)) +xlim(0, 200000)

#Individual Correlations and regressions

#Age vs discount purchase
cor(clean_data$Year_Birth, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Year_Birth, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Year_Birth ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Year_Birth)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Family size vs discount purchase
cor(clean_data$Family_Size, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Family_Size, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Family_Size ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Family_Size)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Marital Status vs discount purchase
cor(clean_data$Marital_Status, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Marital_Status, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Marital_Status ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Marital_Status)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Education vs discount purchase
cor(clean_data$Education, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Education, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Education ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Education)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Location vs discount purchase
cor(clean_data$Country, clean_data$NumDealsPurchases, method ="pearson")
t.test(data = clean_data, clean_data$Country, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Country ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Country)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm)

#Income vs discount purchase
cor(clean_data$Income, clean_data$NumDealsPurchases, method ="pearson", use= "complete.obs")
t.test(data = clean_data, clean_data$Income, clean_data$NumDealsPurchases)
# create the linear model
modOne <- lm(Income ~ NumDealsPurchases,data = clean_data)
# run the linear model
summary(modOne)
clean_data %>% ggplot(aes(x = NumDealsPurchases, y = Income)) +
  geom_point(alpha = I(1/4)) + geom_smooth(method = lm) +ylim(0, 200000)

##Beginning Hypothesis 3



##Beginning Hypothesis 4
