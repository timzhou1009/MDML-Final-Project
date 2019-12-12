# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Modeling ##############
###########################################
# Install required packages
require(tidytext)
require(tidyverse)
require(tm)

dat = read_csv('data/final.csv')

dat <- dat %>% rename(review.count.business = review_count.x,
                      review.count.user = review_count.y,
                      business.star = stars.x,
                      user.star = stars.y) %>% 
  select(-c(X1, X1_1, average_stars.1))
# General guidelines for all models

# Split cleaned data into 90% training and 10% testing using 10 fold cross validation

# train <- 
# test <- 

# train %>% select(text, stars.business)

# use test to predict business rating

# predictions <- 

# compare predicted and actual ratings

# accuracy <- 

#### Feature Selection

# Remove numbers from text
dat$text <- removeNumbers(dat$text)

#  Create bag of words from text column
text_dat <- dat %>% select(text, review_id, business_id)  %>% unnest_tokens(word, text) 

# Remove stop words
data(stop_words)
text_dat <- text_dat %>% anti_join(stop_words)

# Remove words not in english

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

feature_matrix1 <- as.matrix(feature_matrix1)
feature_matrix1 <- as_tibble(cbind(unique(business_words$business_id),feature_matrix1))

# 2. Feature Engineering I; Part of Speech analysis per sentence, use these results to select top k frequent words

# 3. Feature Engineering II; after finishing Part of Speech; extract adjectives only and pick top k adjectives
review_sentence <- dat %>%
  select(review_id, business_id, business.star, text) %>%
  unnest_tokens(sentence, text, token = "sentences")

install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("opneNLP")
library(NLP)
library(tm)  # make sure to load this prior to openNLP
library(openNLP)
library(openNLPmodels.en)



review_string = unlist(dat$text) %>% 
  paste(collapse=' ') %>% 
  as.String

init_s_w = annotate(review_string, list(Maxent_Sent_Token_Annotator(),
                                        Maxent_Word_Token_Annotator()))

pos_res = annotate(review_string, Maxent_POS_Tag_Annotator(), init_s_w)
word_subset = subset(pos_res, type=='word')
tags = sapply(word_subset$features , '[[', "POS")

baby_pos = data_frame(word=review_string[word_subset], pos=tags) %>% 
  filter(!str_detect(pos, pattern='[[:punct:]]'))

# For all 3 feature matrixes, test different values of k and choose the one with the lowest RMSE


# Models - 

# 1 Linear Regression



feature_matrix1 <- feature_matrix1 %>% rename(business_id  = V1)

model_data <- left_join(feature_matrix1, dat[,c('business.star', 'business_id')], by = 'business_id') %>% 
  distinct()


smp_size <- floor(0.90 * nrow(model_data))

train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)

train <- model_data[train_ind, ]
train$business.star <- as.factor(train$business.star)

train <- train %>% select(-business_id)

test <- model_data[-train_ind, ]
test_labels <- test$business.star
test <- test %>% select(-business_id, - business.star)




# 2 Support Vector Regression



# 3 Support Vector Regression with normalized features
# 4 Decision Tree Regression
















