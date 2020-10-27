##################################################
## Project: Data Science Project - Census Data
## Script purpose: Wrapper for all other scripts
## Date: Oct 24, 2020
## Author: Allison Camm
##################################################

# Installs & Imports ------------------------------------------------------


# Note if you would like to run this in your workspace, set your working directory to the locaiton of adult.data 
setwd("~/Desktop/Allison Camm A&F Code")
# Package installation and set-up

# Install packages from CRAN
# Uncomment if you don't have these packages installed 
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("xgboost")
#install.packages('mltools')
#install.packages('pdp')
#install.packages('GGally')

# Imports
library('tidyr')
library('dplyr')
library('xgboost')
library('mltools')
library('data.table')
library('pdp')
library('GGally')

# Read in our data
training <- read.table('./adult.data', sep = ',')
test <- read.table('./adult.test', sep = ',')

input = training
source('./01_clean.R')
training = input

input = test
source('./01_clean.R')
test = input

source('./02_model.R')
source('./03_race_expl.R')
