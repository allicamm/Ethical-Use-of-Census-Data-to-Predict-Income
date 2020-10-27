##################################################
## Project: Data Science Project - Census Data
## Script purpose: Clean data and engineer new features
## Date: Oct 23, 2020
## Author: Allison Camm
##################################################


# Initial Data Prep -------------------------------------------------------

##Clean data
# Rename columns 
input <- input %>% 
  rename(
    age = V1,
    workclass = V2,
    fnlwgt = V3,
    education = V4,
    education_num = V5,
    marital_status = V6,
    occupation = V7,
    relationship = V8,
    race = V9,
    sex = V10,
    capital_gain = V11,
    capital_loss = V12,
    hours_per_week = V13,
    native_country = V14,
    income_flag = V15
  )
# Remove leading or trailing white spaces 
input <- input %>% mutate(across(where(is.factor), stringr::str_trim))
## Exploratory Analysis 

# Get summary statistics & shape 
summary(input)
head(input) 
str(input)

# Everything is factor - make sure to cast numerics as such 
input <- input %>% 
  mutate(age = as.numeric(age),
         fnlwgt = as.numeric(fnlwgt),
         education_num = as.numeric(education_num),
         capital_gain = as.numeric(capital_gain),
         capital_loss = as.numeric(capital_loss),
         hours_per_week = as.numeric(hours_per_week))
summary(input)

# Some columns have ?s - let's make those null for modeling purposes
input <- input %>% mutate_if(is.character, list(~na_if(., "?")))
#also replace all -'s with _
# Some columns have ?s - let's make those null for modeling purposes
input <- input %>% mutate_if(is.character,list(~stringr::str_replace_all(.,'-','_')))

# Its probably not fair to ask if someone under the age of 18 is making 50k
# Filtering them out 
input %>% filter(age>=18) %>% summary()
# Order by the most representative groups of under 18 year olds 
input %>% filter(age<18) %>% arrange(desc(fnlwgt)) %>% head()
# I find it very implausible that 21646 8 year olds have bachelors
# or that 21644 15 year old married women have masters
# I will filter out anyone under the age of 18 

input <- input %>% filter(age>=18)
# Is our data unique?
nrow(input) # 32166

input %>% 
  n_distinct() #32142

# Let's take distinct for modeling purposes - we only lose 10 rows of data 
input <- input %>% distinct()

# Feature Engineering  ----------------------------------------------------

# Quick summary with our newly cleaned data 
summary(input)

# Variables we could create that are tied to income
# employed (1/0)
# college educated (1/0)
# Married/Single
# grouping occupations into industries
# net change in capital
# immigrant status 

## Employment flag 
input %>% select(workclass) %>% distinct()
# Without-pay and never-worked will be considered unemployed 
input <- input %>% 
  mutate(employed = if_else(
    workclass == 'Without-pay' | workclass == 'Never-worked',
    FALSE,
    TRUE
  ))

## College Education 
input %>% select(education) %>% distinct()
#List of college degrees 
degrees = c('Bachelors', 'Masters', 'Some-college', 'Assoc-acdm',
                    'Assoc-voc', 'Doctorate', 'Prof-school')
input <- input %>% 
  mutate(higher_ed = if_else(education %in% degrees, TRUE, FALSE))

## Married or single 
input %>% select(marital_status) %>% distinct()
input <- input %>% mutate(married = if_else(stringr::str_detect(marital_status, 'Married'),
                                      TRUE, FALSE))

# Lets look at the occupations and see if there are natural industries to group by
input %>% select(occupation) %>% distinct()
# With my knowledge these don't seem to have a natural grouping
# Worth exploration in further analyses 

# Net change in capital 
# Are there HHs that have capital gain and capital loss
input %>% filter(capital_gain != 0 & capital_loss!=0)
# No
# Still worth having a range of + and - values so model understands this as one variable with + and - 
input <- input %>% 
  mutate(capital_change = capital_gain - capital_loss) %>% 
  select(-capital_gain, -capital_loss)

# Immigrant Status 
input %>% select(native_country) %>% distinct()
# If any country of origin other than United-States, assume immigrant flag 
input <- input %>% 
  mutate(immigrant_status = if_else(native_country == 'United-States',
                                    FALSE, TRUE))
