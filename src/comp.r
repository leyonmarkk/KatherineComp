### Name:
# Katherine Leyonmark
# Spring 2022
# Senior Comprehensive Project Code

rm(list = ls()) # remove variables stored in memory.
graphics.off()
cat("\014")

# adding libraries here
library(tidyverse)
library(ggplot2)

# install.packages("psych")
library(psych)

# Load CSV data
full_data <- read.csv(file.choose())
View(full_data)
