library(dplyr)
library(tidyverse)
library(randomForest)
library(caret)

esrb_test <- read.csv("test_esrb.csv")
print(esrb_test)
esrb_test <- tibble(esrb_test)
class(esrb_test)
view(esrb_test)

#Check how many rating are there
n_rating <- esrb_test %>%
  select('esrb_rating') %>%
  unique()

#Function for spliting data (90% Train / 10% Test)
split_data <- function(df) {
  set.seed(42)
  n <- nrow(df)
  train_id <- sample(1:n, size = 0.9*n)
  train_df <- df[train_id, ]
  test_df <- df[-train_id, ]
  # Return
  list(training = train_df, 
       testing = test_df) 
}

prep_data <- split_data(esrb_test)
train_data <- prep_data[[1]]
test_data <- prep_data[[2]]

#Train

set.seed(42)

model <- train(esrb_rating ~ alcohol_reference + animated_blood + blood + blood_and_gore + cartoon_violence
               + crude_humor + drug_reference + fantasy_violence + intense_violence + language + mature_humor
               + mild_blood + mild_cartoon_violence + mild_fantasy_violence + mild_language + mild_lyrics
               + mild_suggestive_themes + mild_violence + no_descriptors + nudity + partial_nudity
               + sexual_content + sexual_themes + simulated_gambling + strong_janguage + strong_sexual_content
               + suggestive_themes + use_of_alcohol + use_of_drugs_and_alcohol + violence,
               data = train_data,
               # ML algorithm
               method = 'cforest')

# score and evaluate
p <- predict(model, newdata=test_data)
acc <- mean(p == test_data$esrb_rating)
varImp(model)

# Top 20 Categories Weight
#fantasy_violence         100.000
#violence                  72.526
#blood                     55.951
#no_descriptors            51.700
#blood_and_gore            45.878
#mild_fantasy_violence     24.677
#strong_sexual_content     17.924
#strong_janguage           16.362
#suggestive_themes         14.781
#intense_violence          13.709
#language                  10.358
#mild_blood                 8.601
#use_of_drugs_and_alcohol   6.627
#use_of_alcohol             6.012
#simulated_gambling         4.827
#partial_nudity             4.361
#mild_language              4.309
#drug_reference             3.459
#crude_humor                2.206
#cartoon_violence           1.984
