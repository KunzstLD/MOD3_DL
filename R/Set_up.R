# packages
library(caret)
library(dplyr)
library(data.table)
library(ggplot2)
library(NeuralNetTools)
library(nnet)
library(e1071)

# Path to input data, needs to be specified by the user
data_in <- file.path(getwd(), "data_task")