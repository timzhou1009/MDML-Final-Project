# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Modeling ##############
###########################################

# Install required packages
library(tidytext)
library(tidyverse)

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


#  Create bag of words from text column

text_dat <- dat %>% select(text, review_id, business_id) %>% unnest_tokens(word, text)

# Remove stop words

data(stop_words)
text_dat <- text_dat %>% anti_join(stop_words)




#  pick top k frequent words, calculate frequency of each top k word

# count the 50 most frequent words to view
text_dat_top_k <- text_dat %>% count(word) %>% arrange(desc(n)) %>% slice(1:1000)

# count each word by business id and sort in descending order
business_words <- text_dat %>% count(word, business_id, sort = T)


# group business_words by business id and create column for total # of words in each business_id
total_words <- business_words %>% 
  group_by(business_id) %>% 
  summarize(total = sum(n))

# join business_words and total_words 
business_words <- left_join(business_words, total_words)


# filter business_words to only keep words that belong to top k 
business_words <-  business_words %>% filter(word %in% text_dat_top_k$word)


# 1. Baseline; divide the number of occurence with the total number of occurence for all the top k words

# Resulting training matrix should be size N (number of restaurants) x k 

feature_matrix1 <- business_words %>% group_by(business_id, word) %>% 
  mutate(prop_top_k = n/total) %>% select(business_id, prop_top_k) %>% distinct() %>% arrange(business_id)


# 2. Feature Engineering I; Part of Speech analysis per sentence, use these results to select top k frequent words

# 3. Feature Engineering II; after finishing Part of Speech; extract adjectives only and pick top k adjectives

# For all 3 feature matrixes, test different values of k and choose the one with the lowest RMSE


# Models - 

# 1 Linear Regression
# 2 Support Vector Regression
# 3 Support Vector Regression with normalized features
# 4 Decision Tree Regression

















