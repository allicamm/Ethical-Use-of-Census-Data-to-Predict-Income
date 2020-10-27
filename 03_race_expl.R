##################################################
## Project: Data Science Project - Census Data
## Script purpose: Explore feature distributions based on race
## Date: Oct 25, 2020
## Author: Allison Camm
##################################################

# Note we're going to use the original, non-one hot encoded tables for this analysis for clarity
head(training)
head(test)

tot = rbind(training,test)
# Lets do some indexing based on our most predictive values & race 
# Note we need to use fnlwgt to do so 
# Look only at race and top predictive values other than age

# Let's create some categorical variables based on our most predictive 
training_for_index <- tot %>% 
  mutate(cap_cat = case_when(
    capital_change < 0 ~ '<0',
    capital_change < 20000 ~ '0-20k',
    capital_change >=20000 ~ '>20k'),
    cap_edu = case_when(
      education_num < 5 ~ '<5 years',
      education_num <10 ~ '5-10 years',
      education_num >=10 ~ '10+ years'
    ))
pred_var = c(quo(marital_status), quo(cap_cat), quo(cap_edu))

# Do example of indexing with marital status so we can write all others 

# Table for percent of race in that group 
by_race <- training_for_index %>% 
  # Get total number of that group in each race 
  group_by(race, marital_status) %>% 
  summarize(tot_in_race = sum(fnlwgt)) %>% 
  group_by(race) %>% 
  # get total number of people in race 
  mutate(tot_race = sum(tot_in_race)) %>% 
  ungroup() %>% 
  # Calculate percent 
  mutate(perc_of_race = tot_in_race/tot_race) 

# Get percent of each group in total  
total <- training_for_index %>% 
  group_by(marital_status) %>% 
  summarize(tot_group = sum(fnlwgt)) %>% 
  ungroup() %>% 
  mutate(tot = sum(tot_group)) %>%
  mutate(perc = tot_group/tot)

# Join these two tables and divide percent of race with percent of total to get index 
index_tbl <- by_race %>% 
  inner_join(total, by = 'marital_status') %>% 
  mutate(index = perc_of_race/perc,) %>% 
  select(index, race, marital_status) %>% 
  pivot_wider(values_from = index, names_from = marital_status, values_fill = 0)

# Finally write to csv to be used in deck 
write.table(index_tbl, file = paste0('./', 'marital_status', '_index.csv'))

# Now do this for each variable in our most predictive variables
for(i in seq_along(pred_var)){
  # Table for percent of race in that group 
  by_race <- training_for_index %>% 
    # Get total number of that group in each race 
    group_by(race, !!pred_var[[i]]) %>% 
    summarize(tot_in_race = sum(fnlwgt)) %>% 
    group_by(race) %>% 
    # get total number of people in race 
    mutate(tot_race = sum(tot_in_race)) %>% 
    ungroup() %>% 
    # Calculate percent 
    mutate(perc_of_race = tot_in_race/tot_race) 

  # Get percent of each group in total  
  total <- training_for_index %>% 
    group_by(!!pred_var[[i]]) %>% 
    summarize(tot_group = sum(fnlwgt)) %>% 
    ungroup() %>% 
    mutate(tot = sum(tot_group)) %>%
    mutate(perc = tot_group/tot)

  # Join these two tables and divide percent of race with percent of total to get index 
  index_tbl <- by_race %>% 
    inner_join(total, by = quo_name(pred_var[[i]])) %>% 
    mutate(index = perc_of_race/perc) %>% 
    select(index, race, !!pred_var[[i]]) %>% 
    pivot_wider(values_from = index, names_from = !!pred_var[[i]], values_fill = 0)
  
  # Finally write to csv to be used in deck 
  write.csv(index_tbl, file = paste0('./', quo_name(pred_var[[i]]), '_index.csv')) 
} 

# Let's also look at false negative rates by race
# Start by looking at total false negative rate 
actual_v_predicted = test_w_flag %>% 
  cbind(pred) %>%
  mutate(pred = if_else(pred>=.5, TRUE, FALSE)) 
  
actual_v_predicted %>% summarize(mean(income_flag == TRUE & pred == FALSE))
# Overall false negative rate is 13%
# What happens if we group by race 
# Will need to join to all of test data to get this 
actual_v_predicted %>% group_by(race) %>% summarize(mean(income_flag == TRUE & pred == FALSE))

# True positive rate by race 
actual_v_predicted %>% group_by(race) %>% summarize(mean(income_flag == TRUE & pred == TRUE))

# False positive rates by race 
actual_v_predicted %>% group_by(race) %>% summarize(mean(income_flag == FALSE & pred == TRUE))
# So a white person who does not make 50k is 2.14x more likely to be predicted >50k than a black person

# False positive rates by race 
actual_v_predicted %>% group_by(race, sex) %>% summarize(mean(income_flag == FALSE & pred == TRUE))
# False positive amongst white men is 5% - write this table out 
actual_v_predicted %>% 
  mutate(sex = if_else(sex<1, 'Female', 'Male')) %>%
  group_by(race, sex) %>% 
  summarize(`false positive rate` = mean(income_flag == FALSE & pred == TRUE)) %>%
  write.csv(file = './false_positives.csv') 

# Also write out by just race 
actual_v_predicted %>% 
  group_by(race) %>% 
  summarize(`false positive rate` = mean(income_flag == FALSE & pred == TRUE)) %>%
  write.csv(file = './false_positives_race.csv')
# Whats our overall breakdown of target by race 
actual_v_predicted %>% group_by(race) %>% summarize(mean(income_flag))

# Even though gender was not a top predicted variable, we still see a lot of variance in errors 
# Rerun indexing with gender instead of race 
for(i in seq_along(pred_var)){
  # Table for percent of race in that group 
  by_sex <- training_for_index %>% 
    # Get total number of that group in each race 
    group_by(sex, !!pred_var[[i]]) %>% 
    summarize(tot_in_sex = sum(fnlwgt)) %>% 
    group_by(sex) %>% 
    # get total number of people in race 
    mutate(tot_sex = sum(tot_in_sex)) %>% 
    ungroup() %>% 
    # Calculate percent 
    mutate(perc_of_sex = tot_in_sex/tot_sex) 
  
  # Get percent of each group in total  
  total <- training_for_index %>% 
    group_by(!!pred_var[[i]]) %>% 
    summarize(tot_group = sum(fnlwgt)) %>% 
    ungroup() %>% 
    mutate(tot = sum(tot_group)) %>%
    mutate(perc = tot_group/tot)
  
  # Join these two tables and divide percent of race with percent of total to get index 
  index_tbl <- by_sex %>% 
    inner_join(total, by = quo_name(pred_var[[i]])) %>% 
    mutate(index = perc_of_sex/perc)
  
  # Finally write to csv to be used in deck 
  write.csv(index_tbl, file = paste0('./', quo_name(pred_var[[i]]), '_sex_index.csv')) 
} 
