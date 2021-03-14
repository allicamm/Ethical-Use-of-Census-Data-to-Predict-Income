##################################################
## Project: Data Science Project - Census Data
## Script purpose: Wrapper for all other scripts
## Date: Oct 24, 2020
## Author: Allison Camm
##################################################

# Installs & Imports ------------------------------------------------------


# Note if you would like to run this in your workspace:
# Set your working directory to the location of where you cloned this repo 
setwd("~/Desktop/Ethical Census Data Analysis")
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
training <- read.table('./data/adult.data', sep = ',')
test <- read.table('./data/adult.test', sep = ',')

input = training
source('./src/01_clean.R')
training = input

input = test
source('./src/01_clean.R')
test = input

source('./src/02_model.R')
source('./src/03_race_expl.R')
