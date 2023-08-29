library(caret)
library(tidyverse)
library(readxl)
library(ggplot2)

#Getting data

house <- read_excel("House Price India.xlsx")
view(house)
ggplot(new_house,aes(logprice))+geom_histogram()

#Check n/a (none)

house %>%
  complete.cases() %>%
  mean()

#Create New ID, Changing graph by log
n <- nrow(house)
new_house <- house %>%
  mutate(
    nid = 1:n,
    logprice = log(Price))

#Split Data (85% Train / 15% Test)
split_data <- function(df) {
  set.seed(12)
  n <- nrow(df)
  train_id <- sample(1:n, size = 0.85*n)
  train_df <- df[train_id, ]
  test_df <- df[-train_id, ]
  # return
  list(training = train_df, 
       testing = test_df) 
}

prep_data <- split_data(new_house)
train_df <- prep_data[[1]]
test_df <- prep_data[[2]]

#Train (RANGER Model)
set.seed(42)
rg_model <- train(logprice ~ `number of bedrooms`+`number of bathrooms`+`lot area`+`condition of the house`+`grade of the house`,
                  data = train_df,
                  method = "ranger")

rg_model

#Score model
p <- predict(rg_model, newdata=test_df)

error <- test_df$logprice - p
rmse <- sqrt(mean(error^2))
