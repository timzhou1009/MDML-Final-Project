# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Modeling ##############
###########################################

# Install required packages
require(tidytext)
require(tidyverse)
require(tm)
library(e1071)
require(BBmisc)


# set seed
set.seed(1234)

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


### Pick top k frequent words, calculate frequency of each top k word

# Tried running the models for different levels of k and noted down the rmse for each
# k <- c("50", "100", "500", "1000")

k <- 500
text_dat_top_k <- text_dat %>% count(word) %>% arrange(desc(n)) %>% slice(1:k)


# 1000 most frequent words to view


# Most common words by business id , count for the word in each business id
business_words <- text_dat %>% count(word, business_id, sort = T)


# group business_words by word and create column for total for word occurence in dat
total_words <- business_words %>% 
  group_by(word) %>% 
  summarize(total = sum(n))

business_words <- left_join(business_words, total_words)

# filter business_words to only keep words that belong to top k 
business_words <-  business_words %>% filter(word %in% text_dat_top_k$word) %>% arrange(business_id)



# 1. Baseline; divide the number of occurence with the total number of occurence for all the top k words

# Resulting training matrix should be size N (number of restaurants) x k 

feature_matrix1 <- business_words %>% group_by(business_id, word) %>% 
  mutate(prop_top_k = n/total) %>% select(business_id, prop_top_k) %>% distinct() 

feature_matrix1 <- feature_matrix1 %>% cast_dtm(document = business_id, term = word, value = prop_top_k)



# Models - 



# 2 Support Vector Regression

# Split cleaned data into 90% training and 10% testing 

smp_size <- floor(0.90 * nrow(feature_matrix1))

train_ind <- sample(seq_len(nrow(feature_matrix1)), size = smp_size)

train <- feature_matrix1[train_ind, ]

labels <- dat %>% filter(business_id %in% business_words$business_id) %>% 
  arrange(business_id) %>% select(business_id, business.star) %>% 
  distinct() %>% select(business.star)

train_labels <- labels[train_ind,]
train <- train %>% as.matrix() %>% as.tibble() 
train_ <- train %>% mutate(y = train_labels$business.star)

test <-  feature_matrix1[-train_ind, ] %>% as.matrix() %>% as.tibble()
test_labels <- labels[-train_ind,]

# logistic model

model_log <- nnet::multinom(y ~. , data = train_, MaxNWts = 10000)
pred_log <- predict(model_log, test)


# Run svm model 

model_svm <- svm(train, as.factor(train_labels$business.star), type = "C")
pred_svm <-  predict(model_svm, test)


# Run svm with normalized features 

model_svm_n <- svm(normalize(train), as.factor(train_labels$business.star), type = "C")
pred_svm_n <- predict(model_svm_n, normalize(test))


# Run naive Bayes model


model_nb <- e1071::naiveBayes(train, as.factor(train_labels$business.star))
pred_nb <-  predict(model_nb, test)



# test accuracy of baseline model

rmse_svm <-  Metrics::rmse(test_labels$business.star, as.numeric(as.vector(pred_svm)))
rmse_svm_n <-  Metrics::rmse(test_labels$business.star, as.numeric(as.vector(pred_svm_n)))
rmse_nb <-  Metrics::rmse(test_labels$business.star, as.numeric(as.vector(pred_nb)))
rmse_log <-  Metrics::rmse(test_labels$business.star, as.numeric(as.vector(pred_log)))



# Support Vector Regression with  feature engineering 2

# 2. Only include adjectives and adverbs

text_dat2 <- text_dat %>% 
  left_join(parts_of_speech) %>%
  filter(pos %in% c("Adjective","Adverb")) 




# 1000 most frequent words to view
text_dat_top_k2 <- text_dat2 %>% count(word) %>% arrange(desc(n)) %>% slice(1:k)

# Most common words by business id 
business_words2 <- text_dat2 %>% count(word, business_id, sort = T)


# group business_words by business id and create column for total word occurence in dat
total_words2 <- business_words2 %>% 
  group_by(word) %>% 
  summarize(total = sum(n))

business_words2 <- left_join(business_words2, total_words2)

# filter business_words to only keep words that belong to top k 
business_words2 <-  business_words2 %>% filter(word %in% text_dat_top_k2$word) %>% arrange(business_id)


feature_matrix2 <- business_words2 %>% group_by(business_id, word) %>% 
  mutate(prop_top_k = n/total) %>% select(business_id, prop_top_k) %>% distinct() 

feature_matrix2 <- feature_matrix2 %>% cast_dtm(document = business_id, term = word, value = prop_top_k)

# Split cleaned data into 90% training and 10% testing 

smp_size2 <- floor(0.90 * nrow(feature_matrix2))

train_ind2 <- sample(seq_len(nrow(feature_matrix2)), size = smp_size2)



train2 <- feature_matrix2[train_ind2, ]
labels2 <- dat %>% filter(business_id %in% business_words2$business_id) %>%  arrange(business_id) %>% select(business_id, business.star) %>% distinct() %>% select(business.star)
train_labels2 <- labels2[train_ind2,]
train2 <- train2 %>% as.matrix() %>% as.tibble() 
train2_ <- train2 %>% mutate(y = train_labels2$business.star)

test2 <-  feature_matrix2[-train_ind2, ] %>% as.matrix() %>% as.tibble()
test_labels2 <- labels2[-train_ind2,]

# logistic model

model_log2 <- nnet::multinom(y ~. , data = train2_, MaxNWts = 10000)
pred_log2 <- predict(model_log2, test2)


# Run svm model 

model_svm2 <- svm(train2, as.factor(train_labels2$business.star), type = "C")
pred_svm2 <-  predict(model_svm2, test2)


# Run svm with normalized features 

model_svm_n2 <- svm(normalize(train2), as.factor(train_labels2$business.star), type = "C")
pred_svm_n2 <- predict(model_svm_n2, normalize(test2))


# Run naive Bayes model


model_nb2 <- e1071::naiveBayes(train2, as.factor(train_labels2$business.star))
pred_nb2 <-  predict(model_nb2, test2)



# test accuracy 

rmse_svm2 <- Metrics::rmse(test_labels2$business.star, as.numeric(as.vector(pred_svm2)))
rmse_svm_n2 <-  Metrics::rmse(test_labels2$business.star, as.numeric(as.vector(pred_svm_n2)))
rmse_nb2 <- Metrics::rmse(test_labels2$business.star, as.numeric(as.vector(pred_nb2)))
rmse_log2 <- Metrics::rmse(test_labels2$business.star, as.numeric(as.vector(pred_log2)))


rmse_plot <- c(rmse_svm, rmse_svm_n, rmse_log, rmse_nb, 
               rmse_svm2, rmse_svm_n2, rmse_log2, rmse_nb2)

rmse_plot = as.data.frame(rmse_plot)

rmse_plot <- rmse_plot %>% mutate(id = c("SVM", "SVM - norm", "Multinom-log", "NB",
                                         "SVM", "SVM - norm", "Multinom-log", "NB"),
                                  Feature = c(rep("Top k words", 4), rep("Top k adjectives", 4)))

p <- ggplot(data = rmse_plot, aes (x = id, y = rmse_plot, fill = Feature) ) + 
  geom_bar(stat = "identity", position = "dodge") + ylab("RMSE") + xlab("Classifier")


ggsave("figures/rmse_plot.png")



# Calculating bias for the model with the lowest rmse and highest rmse models

pred_best_mod <- predict(model_log2, as.tibble(as.matrix(feature_matrix2)))

bias_best_mod <- sum(labels2$business.star - as.numeric(pred_best_mod))/length(pred_best_mod)

pred_worst_mod <- predict(model_nb2, as.tibble(as.matrix(feature_matrix2)))

bias_worst_mod <- sum(labels2$business.star - as.numeric(pred_worst_mod))/length(pred_worst_mod)


plot_bias <- c((labels2$business.star - as.numeric(pred_best_mod)),
 (labels2$business.star - as.numeric(pred_worst_mod)))

plot_bias <- plot_bias %>% as.tibble() %>% 
  mutate(mod = c(rep('Multinom-log', 855), rep('Naive Bayes', 855)),
         r = c(seq(1:855), seq(1:855)))


p2 <- ggplot(data = plot_bias, aes(x = r, y = value, fill = mod)) + 
  geom_bar(stat = "identity", position = "dodge") + xlab('Restaurant Index') + ylab('Actual - Predicted')


ggsave('figures/bias_plot.png')







