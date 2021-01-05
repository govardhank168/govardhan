
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#movie rating data
movie_rate_data <- read.csv("/resources/data/data1.csv")

#metadata about the variable
str(movie_rate_data)


#rating distribution
hist(movie_rate_data$rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
movie_rate_data_matrix <- as(movie_rate_data, 'realRatingMatrix')

#Popularity based 

movie_recomm_model1 <- Recommender(movie_rate_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(movie_recomm_model1, movie_rate_data_matrix[413:414], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

movie_recomm_model2 <- Recommender(movie_rate_data_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(movie_recomm_model2, movie_rate_data_matrix[413:414], n=5)
as(recommended_items2, "list")

