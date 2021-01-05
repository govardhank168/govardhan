#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(Matrix)
library(caTools)

#movie rating data
book_rating <- read.csv(file.choose())
book_rating<-book_rating[,-1]
View(book_rating)

#metadata about the variable
#book_rating$Book.Title <- as.numeric(book_rating$Book.Title)
book_rating
str(book_rating)
library(tidyverse)
#book_rating %>% arrangeWindows(desc(Book.Rating))
library(ggplot2)
windows()
ggplot(book_rating[1:20, ], aes(x = Book.Rating, y = Book.Title)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=Book.Rating), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total ratings of the Top books")

library(RColorBrewer)
#rating distribution
hist(book_rating$Book.Rating,xlab = "Book Rating", xlim = ,col=brewer.pal(n = 10, name = "RdBu"), main = "book ratings out of 10")

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rating_matrix <- as(book_rating, 'realRatingMatrix')

#Popularity based 
book_recomm_model1 <- Recommender(book_rating_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, book_rating_matrix[2000:2010], n=5)
as(recommended_items1, "list")

values = sample(x = c(TRUE, FALSE), size = nrow(book_rating_matrix),
                replace = TRUE, prob = c(0.8, 0.2))

train = book_rating_matrix[values, ]
test = book_rating_matrix

#Item_Item Collab
#Building the Model
item_item = Recommender(data = train, method = "IBCF",
                        parameter = list(k = 30, method = "Cosine"))
item_item

#Making Recommendations
# recommending
n_recommended = 6
predictions = predict(object = item_item, newdata = test,
                      n = n_recommended)
predictions

item_pred = function(idNum){
  user_x = predictions@items[[idNum]]
  user_x = predictions@itemLabels[user_x]
  return(data.frame(user_x))
}
item_pred(1)

recc_matrix = sapply(predictions@items, function(x){
  colnames(data)[x]
})

number_of_items = data.frame(table(unlist(recc_matrix, use.names=FALSE)))
sorted = number_of_items[order(number_of_items$Freq, decreasing = T),][1:10,]
kable(sorted)

ggplot(data=sorted, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), color="red", size=3)+
  theme_minimal()+
  xlab("Movie Titles")+
  coord_flip()

#User-User Collab
#Building the Model
user_user = Recommender(data = train, method = "UBCF", parameter = list(k = 30, method = "Cosine"))
user_user

#Making Recommendations
# recommending
n_recommended = 3
predictions2 = predict(object = user_user, newdata = test,
                       n = n_recommended)
predictions2

user_pred = function(idNum){
  user_x = predictions2@items[[idNum]]
  user_x = predictions2@itemLabels[user_x]
  return(data.frame(user_x))
}

user_pred(1)

recc_matrix2 = sapply(predictions2@items, function(x){
  colnames(data)[x]
})

number_of_items = data.frame(table(unlist(recc_matrix2, use.names=FALSE)))
sorted = number_of_items[order(number_of_items$Freq, decreasing = T),][1:10,]
kable(sorted)

ggplot(data=sorted, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), color="red", size=3)+
  theme_minimal()+
  xlab("Movie Titles")+
  coord_flip()

#Comparing Models
#Comparing IBCF and UBCF
set.seed(101)

minimum = min(rowCounts(data))
minimum

evaluation = evaluationScheme(data = data, method = "cross-validation", k = 10, given = 10, goodRating = 3.5)
evaluation

ev_train = getData(evaluation, "train")
ev_known = getData(evaluation, "known")
ev_unknown = getData(evaluation, "unknown")

# Item
item_model = Recommender(data = ev_train, method = "IBCF", parameter = list(method = "Cosine"))

item_model_pred = predict(object = item_model, newdata = ev_known, n = 10, type = "ratings")

item = calcPredictionAccuracy(x = item_model_pred, data = ev_unknown, byUser = FALSE)


# User
user_model = Recommender(data = ev_train, method = "UBCF", parameter = list(method = "Cosine"))

user_model_pred = predict(object = user_model, newdata = ev_known, n = 10, type = "ratings")

user = calcPredictionAccuracy(x = user_model_pred, data = ev_unknown, byUser = FALSE)


# Comparison
kable(rbind(item, user))

eval_sets = evaluationScheme(data = data, method = "cross-validation", k = 4, given = 10, goodRating = 3.5)
I_results = evaluate(x = eval_sets, method = "IBCF", n = seq(10, 100, 10))

kable(head(getConfusionMatrix(I_results)[[1]]))

U_results = evaluate(x = eval_sets, method = "UBCF", n = seq(10, 100, 10))

kable(head(getConfusionMatrix(U_results)[[1]]))

#kable(head(getConfusionMatrix(U_results)[[1]]))
plot(I_results, annotate = TRUE, main = "ROC curve of IBCF")

#Comparing Different Model Approaches
mult_models = list(
  IBCF_cos = list(name = "IBCF", param = list(method = "Cosine")),
  IBCF_pearson = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "Cosine")),
  UBCF_pearson = list(name = "UBCF", param = list(method = "pearson")),
  Random = list(name = "RANDOM", param = NULL),
  Popular = list(name = "POPULAR", param = NULL)
)

# Testing models
models = evaluate(eval_sets, mult_models, n= c(1, 5, seq(10, 100, 10)))

# Plotting models
plot(models, annotate = T, legend="topleft")

#plot(models, "prec/rec", annotate = F, main="Precision/Recall", legend="topright")





#--------------------------------------------------------------
## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering
book_recomm_model2 <- Recommender(book_rating_matrix, method="IBCF")

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, book_rating_matrix[2000:2010], n=5)
as(recommended_items2, "list")

#Heatmap of Movie Ratings
windows()
image(book_rating_matrix[3:2000], axes = FALSE, main = "Heatmap of the first 2000 rows and 3 columns")
#Performing Data Preparation
book_ratings <- book_rating_matrix[rowCounts(book_rating_matrix) > 50,
                              colCounts(book_rating_matrix) > 3]
book_ratings

minimum_book<- quantile(rowCounts(book_ratings), 0.98)
minimum_ratings <- quantile(colCounts(book_ratings), 0.98)
image(book_rating_matrix[rowCounts(book_rating_matrix) > minimum_book,
                    colCounts(book_rating_matrix) > minimum_ratings],
      main = "Heatmap of the top ratings and books")

average_ratings <- rowMeans(book_rating_matrix)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per books")

#Data Normalization
#normalized_ratings <- normalize(book_rating)
sum(rowMeans(book_rating_matrix) > 0.01)
image(book_rating_matrix[rowCounts(book_rating_matrix) > minimum_book,
                         colCounts(book_rating_matrix) > minimum_ratings],
      main = "Book titles of the Top ratings")

#Performing Data Binarization
binary_minimum_book <- quantile(rowCounts(book_rating_matrix), 0.95)
binary_minimum_rating <- quantile(colCounts(book_rating_matrix), 0.95)
#movies_watched <- binarize(movie_ratings, minRating = 1)
good_rated_books <- binarize(book_rating_matrix, minRating = 5)
image(good_rated_books[rowCounts(book_rating_matrix) > binary_minimum_book,
                       colCounts(book_rating_matrix) > binary_minimum_rating],
      main = "Heatmap of the top rating and books")

#Collaborative Filtering System
sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(book_rating_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- book_rating_matrix[sampled_data, ]
testing_data <- book_rating_matrix[!sampled_data, ]


#Building the Recommendation System using R
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters


recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
Books_Rated1 <- predicted_recommendations@itemLabels[user1]
Books_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2

recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]