##### Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Exploration ##################
###############################################

# Packages
require(readr)
require(tidytext)
require(tidyverse)
require(dplyr)
require(ggplot2)        #This package is used for visualizations (chart/graph plotting functions)
require(ggmap)          #This package is used for map functions 
require(maps)           #This package is used for map functions
require(leaflet)        #This package is used for plotting maps
require(wordcloud)      #This package is used to generate word clouds
require(tm)             #This is a text mining package used in the process of generating word clouds

### Reading in Final Joined CSV
dat = read_csv('data/final.csv')

dat <- dat %>% rename(review.count.business = review_count.x,
                      review.count.user = review_count.y,
                      business.star = stars.x,
                      user.star = stars.y) %>% 
  select(-c(X1, X1_1, average_stars.1))

write_csv(dat, 'data/dat_clean.csv')

# Inspecting NAs

# NA_values <- is.na(dat)
# NA_Count <- apply(NA_values,2,sum)
# NA_Count_df <- NA_Count[NA_Count==dim(dat)[1]]

#The following columns are irrelevant for restaurants and are completely empty
# names(NA_Count_df)
colMeans(is.na(dat)) 

### Where are Top 10 Cities in this Data?
# plot longitude and latitude
dat$longitude = round(dat$longitude, 1)
dat$latitude = round(dat$latitude, 1)

top_cities <- dat %>% 
  group_by(city, longitude, latitude) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(12) 

map1 <- leaflet(top_cities) %>% 
  addTiles() %>% setView(lng = -96.503906, lat = 38.68551, zoom = 4) %>%addCircleMarkers(lng = ~longitude, lat = ~latitude, weight = 0, radius=~n/1000+10, fillOpacity = 0.4 , color="Magenta" , popup = ~city) 
map1

### Top categories of Top-Rated Restaurants
## note that ~80% is have NAs in the category section 

top_rated_categories <- dat %>% filter(business.star> 3.5) %>% select(name,categories) %>% distinct()
top_rated_categories[complete.cases(top_rated_categories),]
docs <- Corpus(VectorSource(top_rated_categories$categories)) 

#Converting to lower case, removing stopwords, punctuation and numbers
docs <- tm_map(docs, removePunctuation)    
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, c(stopwords("english"),"s","ve"))  


#Term Document Matrix
Ngrams <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
tdm <- TermDocumentMatrix(docs, control = list(tokenize = Ngrams))
freq = sort(rowSums(as.matrix(tdm)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df,25) # Top 25 categories

### Word Cloud for Word Frequency in Review Text
build_word_cloud <- function(range)
{
  if(range=="lower")
    wordcloud_restaurants <- dat %>% filter(business.star<3) %>% select(business_id,name)
  else
    wordcloud_restaurants <- dat %>% filter(business.star>3.5) %>%  select(business_id,name)
  wordcloud_restaurants_reviews <- dat %>% filter(business_id %in% wordcloud_restaurants$business_id) 
  dir(wordcloud_restaurants_reviews$text)   
  docs <- Corpus(VectorSource(wordcloud_restaurants_reviews$text)) 
  #Converting to lower case, removing stopwords, punctuation and numbers
  docs <- tm_map(docs, removePunctuation)    
  #docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, tolower)   
  docs <- tm_map(docs, removeWords, c(stopwords("english"),"s","ve"))  
  
  #Term Document Matrix
  Ngrams <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
  tdm <- TermDocumentMatrix(docs, control = list(tokenize = Ngrams))
  freq = sort(rowSums(as.matrix(tdm)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  wordcloud(freq.df$word,freq.df$freq,min.freq=5,max.words=100,random.order = F, colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.25))
}

# Word Cloud for Reviews of Restaurants with an average Rating of below 3
build_word_cloud("lower")

# Word Cloud for Reviews of Restaurants with an average Rating of above 3.5
build_word_cloud("higher")

# https://rpubs.com/shreyaghelani/234363


### An Analysis on the Effect of Price Perception on User Ratings
# https://rpubs.com/mjfii/value-bias-analysis
### Tidy Sentiment Analysis

# Creating one-row-per-term-per-document
review_words <- dat %>%
  select(review_id, business_id, business.star, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$")) # removed “stopwords” 

review_sentiment = review_words %>% 
  inner_join(get_sentiments("afinn"), by = 'word') %>% 
  group_by(review_id, business.star) %>%
  summarize(sentiment = mean(value))

ggplot(review_sentiment, aes(business.star, sentiment, group = business.star)) +
  geom_boxplot() + ylab("Average Sentiment Score")
# Confirms that the higher the store, more positive the words used in sentiment analysis.

### Which words are positive or negative?
review_words_counted <- review_words %>%
  count(review_id, business_id, business.star, word) %>%
  ungroup()

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(business.star)) %>%
  ungroup()

# Look at words that appear in at least 200 reviews. Rare words have noisier measurem,ent (few good or bad reviews could shift balance). They are less likely be useful in classifying reviews or text.
# Filtering also for ones that appear in at least 10 other businesses
word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

word_summaries_filtered

# What were the most positive words?
word_summaries_filtered %>%
  arrange(desc(average_stars))

# Negative words?

word_summaries_filtered %>%
  arrange(average_stars)

# Plot by Frequency
ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point(size = 0.05) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  xlab("# of reviews") +
  ylab("Average Stars")

# Food is neutral word
### Comparing to sentiment Analysis
words_afinn <- word_summaries_filtered %>%
  inner_join(get_sentiments('afinn'))

ggplot(words_afinn, aes(value, average_stars, group = value)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")

ggplot(words_afinn, aes(value, average_stars)) +
  geom_point(size = 0.05) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1, size=2) +
  scale_x_log10() +
  xlab("AFINN Sentiment Score") +
  ylab("Average Yelp Business Stars")

ggplot(words_afinn, aes(reviews, average_stars, color = value)) +
  geom_point(size = 0.05) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1,size=2) +
  scale_x_log10() +
  xlab("Number of Reviews") +
  ylab("Average Yelp Business Stars")

# http://varianceexplained.org/r/yelp-sentiment/


# https://github.com/mjfii/Yelp-Value-Bias-Analysis



# https://github.com/Yelp-Kaggle/Yelp
#https://nativeatom.github.io/document/Yelp.pdf

# https://github.com/AmiGandhi/Yelp-User-Rating-Prediction-using-NLP-and-Naive-Bayes-algorithm-and-Restaurant-Recommender

