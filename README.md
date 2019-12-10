# MDML-Final-Project
Messy Data and Machine Learning Final Project 2019 Fall

Predict Bias for Yelp Businesses from review text using NLP

We will look at Yelp Dataset Challenge’s review dataset to explore if we can detect bias in Yelp reviews. 
Yelp’s rating algorithm determines the overall rating without taking into account the reviewers’ special characteristics,
while different people may have diverse rating tendency. Some people are reluctant to give a five star while some others 
are the opposite. Even when two people have the same view on a restaurant, one may tend to give it a five star while 
the other follow his or her own habit and give three star.

Steps
Predict ratings for a unique user-business from review text using NLP
1. Feature Engineering: Create bag of words from the top frequent words in all raw text reviews, or top
frequent words/adjectives from results of Part-of-Speech analysis.
2. Having the bag of words, we treat the problem of predicting a business star as a regression problem.
We will choose four learning models: (i) Linear Regression; (ii) Support Vector Regression; (iii) Support
Vector Regression with normalized features; and (iv) Decision Tree Regression.
3. Look at RMSE as a measure to choose the best model for prediction.
4. Do the prediction and compare the predicted ratings to actual ratings to calculate bias

More specifically,

Dataset​ — features
1. review_id 
2. user_id
3. business_id 
4. stars
5. date 
6. text 
7. useful 
8. funny 
9. cool
