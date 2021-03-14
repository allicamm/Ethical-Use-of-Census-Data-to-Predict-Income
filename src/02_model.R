##################################################
## Project: Data Science Project - Census Data
## Script purpose: Create model & explore feature effects 
## Date: Oct 24, 2020
## Author: Allison Camm
##################################################

# We have our training set train from 01_clean 
# Need to do some basic changes for modeling

# Make a binary column from income flag and get rid of fnlwgt, which is not predictive
training_w_flag <- training %>% 
  mutate(income_flag = if_else(income_flag == '<=50K', FALSE, TRUE)) %>% 
  select(-fnlwgt) %>% 
# Make sex binary where 1 equals male 
  mutate(sex = if_else(sex == 'Male', 1, 0)) 

# One hot encode all string variables 
# First cast all strings as factors 
columns <-  c('workclass',
           'education', 'marital_status', 'occupation',
           'relationship','race', 'native_country')
training_w_flag[columns] <- lapply(training_w_flag[columns], factor)
training_final <- one_hot(data.table(training_w_flag), cols = columns) %>% 
  select(-income_flag)
training_labs <- training_w_flag['income_flag']
# Create matrix for xgboost
train_mat <- xgb.DMatrix(data = as.matrix(training_final), label = as.matrix(training_labs))
# Train the model
model <- xgboost(data = train_mat, objective = "binary:logistic", nround = 10)

# Bring in our test data 
# We need to rerun all of data prep - we already did the cleaning, but need to redo the modeling prep
# Make a binary column from income flag and get rid of fnlwgt, which is not predictive
# note there's a . at the end of this column 
test_w_flag <- test %>% 
  mutate(income_flag = if_else(income_flag == '<=50K.', FALSE, TRUE)) %>% 
  select(-fnlwgt) %>% 
  # Make sex binary where 1 equals male 
  mutate(sex = if_else(sex == 'Male', 1, 0)) 

# One hot encode all string variables 
# First cast all strings as factors 
columns <-  c('workclass',
              'education', 'marital_status', 'occupation',
              'relationship','race', 'native_country')
test_w_flag[columns] <- lapply(test_w_flag[columns], factor)
test_final <- one_hot(data.table(test_w_flag), cols = columns) %>% 
  select(-income_flag)
test_mat <- xgb.DMatrix(data = as.matrix(test_final))
test_labs <- as.list(test_w_flag['income_flag'])
# Predict with our test data
pred <- predict(model, test_mat)
# So our error will be if pred >= .5 but label is False 
data.frame(actuals = test_labs, prediction = pred) %>% 
  transmute(diff = (prediction>=.5)!=income_flag) %>% 
  summarize(error = mean(diff))
# Error rate is 16%, which is about what we saw with our training set 


importance_matrix <- xgb.importance(feature_names = colnames(training_final), model = model)
importance_matrix
xgb.plot.importance(importance_matrix = importance_matrix)

# 3 features really dominate feature importance - look at pdps for them 
# Marital Status-civ spouse (married to a civilian)
partial(model, train = training_final, pred.var = 'marital_status_Married_civ_spouse', plot = TRUE)
# More likely if married to have over 50k

# Capital Change
# NOTE this is where pdf change in likelihood - age was saved from
partial(model, train = training_final, pred.var = 'capital_change', plot = TRUE, grid.resolution = 50)
# Least likely if you have capital change near 0 

# Education_num
partial(model, train = training_final, pred.var = 'education_num', plot = TRUE)
# More education, more likely to be making more than 50k

# Additionally take a look at age, which is in top 5 
# NOTE this is where pdf change in likelihood - age was saved from
partial(model, train = training_final, pred.var = 'age', plot = TRUE)
# Large jump when you're over 30, levels off with age 

# Race is a protected class and still near the top - see its affects 
partial(model, train = training_final, pred.var = 'race_White', plot = TRUE)
# Slightly more likely to be over 50k if white - how do other more predictive features trend with race 

