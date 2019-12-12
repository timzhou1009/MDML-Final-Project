# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Modeling ##############
###########################################
# Install required packages
require(tidytext)
require(tidyverse)
require(tm)

# Read data into R

dat = read_csv('data/final.csv')

# rename variables 

dat <- dat %>% rename(review.count.business = review_count.x,
                      review.count.user = review_count.y,
                      business.star = stars.x,
                      user.star = stars.y) %>% 
  select(-c(X1, X1_1, average_stars.1))




#### Feature Selection

# Remove numbers from text
dat$text <- removeNumbers(dat$text)

#  Create bag of words from text column
text_dat <- dat %>% select(text, review_id, business_id)  %>% unnest_tokens(word, text) 

# Remove stop words
data(stop_words)
text_dat <- text_dat %>% anti_join(stop_words)

# Remove neutral words 

# neutral_words <- as.tibble(c('dinner', 'dish', 'dishes' ,'drink', 'pm', 'red', 
               #    'main', 'plates', 'entrees', 'tonight', 'cup', 'le', 'fork', 'bought',
                #   'im', 'eat', 'drinks', 'day', 'meal'))

# neutral_words <- neutral_words  %>% rename(word = value)
  
# text_dat <- text_dat %>% anti_join(neutral_words)


### Pick top k frequent words, calculate frequency of each top k word

# 1000 most frequent words to view
text_dat_top_k <- text_dat %>% count(word) %>% arrange(desc(n)) %>% slice(1:1000)

# Most common words by business id 
business_words <- text_dat %>% count(word, business_id, sort = T)

# Top Word Occurence by Restaurant and Total
# group business_words by business id and create column for total # of words in each business_id
total_words <- business_words %>% 
  group_by(business_id) %>% 
  summarize(total = sum(n))

business_words <- left_join(business_words, total_words)

# filter business_words to only keep words that belong to top k 
business_words <-  business_words %>% filter(word %in% text_dat_top_k$word) %>% arrange(business_id)

# 1. Baseline; divide the number of occurence with the total number of occurence for all the top k words

# Resulting training matrix should be size N (number of restaurants) x k 

feature_matrix1 <- business_words %>% group_by(business_id, word) %>% 
  mutate(prop_top_k = n/total) %>% select(business_id, prop_top_k) %>% distinct() 

feature_matrix1 <- feature_matrix1 %>% cast_dtm(document = business_id, term = word, value = prop_top_k)

# 2. Only include adjectives and adverbs

text_dat <- text_dat %>% 
left_join(parts_of_speech) %>%
  filter(pos %in% c("Adjective","Adverb")) 

text_dat_top_k2 <- text_dat %>% count(word) %>% arrange(desc(n)) %>% slice(1:1000)

# Most common words by business id 
business_words2 <- text_dat %>% count(word, business_id, sort = T)

# Top Word Occurence by Restaurant and Total
# group business_words by business id and create column for total # of words in each business_id
total_words2 <- business_words2 %>% 
  group_by(business_id) %>% 
  summarize(total = sum(n))

business_words2 <- left_join(business_words2, total_words2)

# filter business_words to only keep words that belong to top k 
business_words2 <-  business_words2 %>% filter(word %in% text_dat_top_k2$word) %>% arrange(business_id)

# 1. Baseline; divide the number of occurence with the total number of occurence for all the top k words

# Resulting training matrix should be size N (number of restaurants) x k 

feature_matrix2 <- business_words2 %>% group_by(business_id, word) %>% 
  mutate(prop_top_k = n/total) %>% select(business_id, prop_top_k) %>% distinct() 

feature_matrix2 <- feature_matrix2 %>% cast_dtm(document = business_id, term = word, value = prop_top_k)



# Models - 

# 1 Linear Regression

# 2 Support Vector Regression

# Split cleaned data into 90% training and 10% testing using 10 fold cross validation

smp_size <- floor(0.90 * nrow(feature_matrix1))

train_ind <- sample(seq_len(nrow(feature_matrix1)), size = smp_size)

train <- feature_matrix1[train_ind, ]
labels <- dat %>% arrange(business_id) %>% select(business_id, business.star) %>% distinct() %>% select(business.star)
train_labels <- labels[train_ind,]
train <- train %>% as.matrix() %>% as.tibble() %>%  mutate(y = train_labels$business.star)


test <-  feature_matrix1[-train_ind, ] %>% as.matrix() %>% as.tibble()
test_labels <- labels[-train_ind,]


library(caret)
model <- train(y ~., data = train, method = 'svmLinear3')
pred <-  predict(model, test)
pred <- round(pred*2)/2




Metrics::accuracy(test_labels$business.star, pred)


# 3 Support Vector Regression with  feature engineering 2

smp_size2 <- floor(0.90 * nrow(feature_matrix2))

train_ind2 <- sample(seq_len(nrow(feature_matrix2)), size = smp_size2)

train2 <- feature_matrix2[train_ind2, ]
labels2 <- dat %>% arrange(business_id) %>% select(business_id, business.star) %>% distinct() %>% select(business.star)
train_labels2 <- labels2[train_ind2,]
train2 <- train2 %>% as.matrix() %>% as.tibble() %>%  mutate(y = train_labels2$business.star)


test2 <-  feature_matrix2[-train_ind2, ] %>% as.matrix() %>% as.tibble()
test_labels2 <- labels2[-train_ind2,]


library(caret)
model2 <- train(y ~., data = train2 , method = 'svmLinear3')
pred2 <-  predict(model2, test2)
pred2 <- round(pred2*2)/2




Metrics::accuracy(test_labels2$business.star, pred2)
# 4 Decision Tree Regression
