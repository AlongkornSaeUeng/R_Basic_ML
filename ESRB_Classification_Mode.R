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
n_col <- esrb_test %>%
  select('console') %>%
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
# acc = 0.74
varImp(model)

#new data from ESRB [New Data Testing Model]
new_test <- data.frame (
  tiltle = c('Starfield','Minecraft','Just Dance 2024','Final fantasy XVI','Valorant','Super Mario Bros. Wonder'
             ,'The Legend of Zelda: Tears of the Kingdom','Persona 5'),
  console = c(0,0,1,1,0,1,1,1),
  alcohol_reference = c(0,0,0,0,0,0,0,0),
  animated_blood = c(0,0,0,0,0,0,0,0),
  blood = c(1,0,0,0,1,0,0,1),
  blood_and_gore = c(0,0,0,1,0,0,0,0),
  cartoon_violence = c(0,0,0,0,0,0,0,0),
  crude_humor = c(0,0,0,0,0,0,0,0),
  drug_reference = c(0,0,0,0,0,0,0,1),
  fantasy_violence = c(0,1,0,0,0,0,1,0),
  intense_violence = c(0,0,0,0,0,0,0,0),
  language = c(0,0,0,0,1,0,0,0),
  lyrics = c(0,0,0,0,0,0,0,0),
  mature_humor = c(0,0,0,0,0,0,0,0),
  mild_blood = c(0,0,0,0,0,0,0,0),
  mild_cartoon_violence = c(0,0,0,0,0,0,0,0),
  mild_fantasy_violence = c(0,0,0,0,0,1,0,0),
  mild_language = c(0,0,0,0,0,0,0,0),
  mild_lyrics = c(0,0,1,0,0,0,0,0),
  mild_suggestive_themes = c(0,0,0,0,0,0,1,0),
  mild_violence = c(0,0,0,0,0,0,0,0),
  no_descriptors = c(0,0,0,0,0,0,0,0),
  nudity = c(0,0,0,0,0,0,0,0),
  partial_nudity = c(0,0,0,1,0,0,0,1),
  sexual_content = c(0,0,0,0,0,0,0,0),
  sexual_themes = c(0,0,0,1,0,0,0,1),
  simulated_gambling = c(0,0,0,0,0,0,0,0),
  strong_janguage = c(1,0,0,1,0,0,0,1),
  strong_sexual_content = c(0,0,0,0,0,0,0,0),
  suggestive_themes = c(1,0,0,0,0,0,0,0),
  use_of_alcohol = c(0,0,0,0,0,0,0,0),
  use_of_drugs_and_alcohol = c(1,0,0,0,0,0,0,0),
  violence = c(1,0,0,1,1,0,0,1),
  esrb_rating = c('M','ET','E','M','T','E','ET','M')
)

# score and evaluate
p_new <- predict(model, newdata=new_test)
acc_new <- mean(p_new == new_test$esrb_rating)
 
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

#result prediction
#esrb_rating = c('M','ET','E','M','T','E','ET','M')
#predict_esrb_rating = T  ET T  M  T  E  ET T 
# acc = 0.74
# newacc = 0.625 
# the dataset of new test is too small but the result is satisfy
