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
comp_data <- select(full_data, -(ID), -(Dt_Customer), -(Recency), -(NumWebVisitsMonth), -(AcceptedCmp3), -(AcceptedCmp4), -(AcceptedCmp5), -(AcceptedCmp1), -(AcceptedCmp2), -(Response), -(Complain))

#filter out a subset of data with only numerical values
numeric <- select(comp_data, -(Country), -(Education), -(Marital_Status))

#full correlation set via cor plot
corPlot(numeric)
