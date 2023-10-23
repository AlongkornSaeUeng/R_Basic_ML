library(caret)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(glmnet)
library(rpart)
library(rpart.plot)

#Getting data and look at the data

house <- read_excel("House_Price_India.xlsx")
glimpse(house)
view(house)
save.csv

#Check n/a (none)

house %>%
  complete.cases() %>%
  mean()

#Get number of number row (get how many data are there)
n <- nrow(house)

#Check the price data curve by plotting graph
ggplot(house,aes(Price))+geom_histogram()

#Create New Column (new_id, logprice), Changing graph to log scale
new_house <- house %>%
  mutate(
    nid = 1:n,
    logprice = log(Price))
glimpse(new_house)
view(new_house)
write.csv(new_house, "indian_house.csv",row.names=FALSE)

#Plotting graph(logscale)
ggplot(new_house,aes(logprice))+geom_histogram()


#Resampling
control <- trainControl(method='boot', 
                        number=100, 
                        verboseIter = TRUE)

#Split Data Function (85% Train / 15% Test)
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

#use root as standard of train 
metric <- "RMSE"

#Split data 
prep_data <- split_data(new_house)
train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

#Hypertunning
glmnet_grid <- expand.grid(alpha = 0, lambda = c(0.1, 0.2, 0.3))
rpart_grid <- expand.grid(cp = c(0.25,0.35,0.5))

#Train
set.seed(42)

#Ridge Model
ridge_model <- train(logprice ~ number_of_bedrooms + number_of_bathrooms + living_area + lot_area +
                 number_of_floors + number_of_views + condition_of_the_house + grade_of_the_house +
                 Area_of_the_house_excluding_basement + Area_of_the_basement +
                 Built_Year + Renovation_Year + Postal_Code + Number_of_schools_nearby + Distance_from_the_airport,
               data = train_data,
               method = 'glmnet',
               tuneGrid = glmnet_grid,
               metric = 'RMSE',
               trControl = control)

#Linear Regression Model
lm_model <- train(logprice ~ number_of_bedrooms + number_of_bathrooms + living_area + lot_area +
                       number_of_floors + number_of_views + condition_of_the_house + grade_of_the_house +
                       Area_of_the_house_excluding_basement + Area_of_the_basement +
                       Built_Year + Renovation_Year + Postal_Code + Number_of_schools_nearby + Distance_from_the_airport,
                     data = train_data,
                     method = 'lm',
                     metric = 'RMSE',
                     trControl = control)

#Decision Tree Model
dt_model <- train(logprice ~ number_of_bedrooms + number_of_bathrooms + living_area + lot_area +
                    number_of_floors + number_of_views + condition_of_the_house + grade_of_the_house +
                    Area_of_the_house_excluding_basement + Area_of_the_basement +
                    Built_Year + Renovation_Year + Postal_Code + Number_of_schools_nearby + Distance_from_the_airport,
                  data = train_data,
                  method = 'rpart',
                  metric = 'RMSE',
                  tuneGrid = rpart_grid,
                  trControl = control)

#Test
p_ridge_model <- predict(ridge_model, newdata=test_data)
p_lm_model <- predict(lm_model, newdata=test_data)
p_dt_model <- predict(dt_model, newdata=test_data)

#Error & Compare
error_rg <- test_data$logprice - p_ridge_model
error_lm <- test_data$logprice - p_lm_model
error_dt <- test_data$logprice - p_dt_model

RMSE_rg <- (rmse <- sqrt(mean((error_rg)**2)))
RMSE_lm <- (rmse <- sqrt(mean((error_lm)**2)))
RMSE_dt <- (rmse <- sqrt(mean((error_dt)**2)))

#Decision Tree
tree_plot <- rpart.plot(dt_model$finalModel)

#Compare
list_models <- list(ridge_model,lm_model,dt_model)

result <- resamples(list_models)
summary(result)

varImp(lm_model)
