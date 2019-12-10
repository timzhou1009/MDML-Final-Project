# Christina Ho, Tim Chou, Ria Pinjani #####
########### Data Exploration ##############
###########################################





#Install required packages 
# install.packages('jsonlite')
require(jsonlite)
require(lubridate)


# user = stream_in(file('data/user.json')) 
user = data.frame(user)

# review = stream_in(file('data/review.json')) 
review = data.frame(review)

# business = stream_in(file('data/business.json')) 
business = data.frame(business)


# Subset business data for only businesses that contain the word
# restaurant in the column categories
restaurant <- business[c(grep("Restaurant",business$categories), 
                         grep("Restaurants",business$categories)),]


# Randomly choose 1000 restaurants
samp <- sample(nrow(restaurant),1000)
restaurant_data = restaurant[samp,]

# write dataset to csv file
write_csv(restaurant_data,'data/restaurant_data.csv')


# Read in the saved csv file
restaurant_data = read_csv('data/restaurant_data.csv')


# Select rows from the dataset reviews for which business id 
# matches those in restaraunt_data
id = unique(restaurant_data$business_id)
review_chosen <- review[review$business_id %in% id,]

# Filter for only most recent 4 years of reviews
review_dat = review_chosen %>% filter(year(date) %in% c(2015, 2016, 2017, 2018))

# Select rows from the dataset user for which user id matches those in review_dat
user_id = unique(review_dat$user_id)
user_chosen <- user[user$user_id %in% user_id,]

# Merge User information with Review information by user id
review_final = merge(review_dat, user_chosen[, c("user_id", "review_count", "yelping_since",
                                                 "average_stars", "elite", "fans", "average_stars")], by = "user_id", all = T)


# write dataset to csv file
write_csv(review_final,'data/review_final.csv')


# Read in the saved csv file
review_final <- read_csv('data/review_final.csv')


# Merge user & review merged file above file with restaurant data (subset of dataset business) by business id
final = merge(review_final, restaurant_data[, c("business_id", "name", "city", "state", "stars")], by = "business_id", all = T)


# write dataset to csv file
write_csv(final,'data/final.csv')





