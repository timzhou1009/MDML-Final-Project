# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Modeling ##############
###########################################

# General guidelines for all models

# Split cleaned data into 90% training and 10% testing using 10 fold cross validation

# train <- 
# test <- 

# train %>% select(text, stars.business)

# use test to predict business rating

# predictions <- 

# compare predicted and actual ratings

# accuracy <- 




# Feature Selection


#  Create bag of words from text column, pick top k frequent words, calculate frequency of each top k word

# 1. Baseline; divide the number of occurence with the total number of occurence for all the top k words

# Resulting training matrix should be size N (number of restaurants) x k 


# 2. Feature Engineering I; Part of Speech analysis per sentence, use these results to select top k frequent words

# 3. Feature Engineering II; after finishing Part of Speech; extract adjectives only and pick top k adjectives

# For all 3 feature matrixes, test different values of k and choose the one with the lowest RMSE


# Models - 

# 1 Linear Regression
# 2 Support Vector Regression
# 3 Support Vector Regression with normalized features
# 4 Decision Tree Regression

















