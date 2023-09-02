library(plyr)
library(readr)
library(dplyr)
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(ggplot2)     # for awesome plotting
# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip,quietly = TRUE)         # for feature importance
library(pdp,quietly = TRUE)         # for feature effects


# set working directory
setwd("C:/Users/xxxxx/")

# Load your CSV data
#data <- read.csv("dataname.csv")


