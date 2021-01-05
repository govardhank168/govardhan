install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
install.packages("e1071")
install.packages("modeldata")
install.packages("gmodels")

library(C50)
library(tree)
library(e1071)
library(modeldata)
library(gmodels)
data()
data("iris")
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building model on training data 
irisc5.0_train <- C5.0(iris_train[,-5.0],iris_train$Species)
windows()
plot(irisc5.0_train) # Tree graph
# Training accuracy
pred_train <- predict(irisc5.0_train,iris_train)

mean(iris_train$Species==pred_train) # 97.33% Accuracy

library(caret)
confusionMatrix(pred_train,iris_train$Species)

predc5.0_test <- predict(irisc5.0_train,newdata=iris_test) # predicting on test data
mean(predc5.0_test==iris_test$Species) # 94.66% accuracy 
confusionMatrix(predc5.0_test,iris_test$Species)
library(gmodels)
# Cross tablez
CrossTable(iris_test$Species,predc5.0_test)

##### Using tree function 
library(tree)
# Building a model on training data 
iris_tree <- tree(Species~.,data=iris_train)
plot(iris_tree)
text(iris_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(iris_tree,newdata=iris_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(iris_tree,newdata=iris_test)

# for (i in 1:nrow(pred_tree)){
#   pred_tree[i,"final"]<-ifelse(pred_tree[i,"setosa"]>0.5,"setosa",ifelse(pred_tree[i,"versicolor"]>0.5,"versicolor","virginica"))
# }
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==iris_test$Species) # Accuracy = 94.66%
CrossTable(iris_test$Species,pred_tree$final)

####### CART #############
library(rpart)
CompanyData <- read.csv(file.choose())
View(CompanyData)
names(CompanyData)
CompanyData[CompanyData$ShelveLoc==1,]<-"Bad"
CompanyData[CompanyData$ShelveLoc==2,]<-"Good"
CompanyData[CompanyData$ShelveLoc==3,]<-"Medium"
CompanyData$ShelveLoc<-as.factor(CompanyData$ShelveLoc)
CompanyData_Bad<-CompanyData[CompanyData$Bad=="Bad",] # 50
CompanyData_Good <- CompanyData[CompanyData$Good=="Good",] # 50
CompanyData_Medium <- CompanyData[CompanyData$Medium=="Medium",] # 50
CompanyData_train <- rbind(CompanyData_Bad[1:200,],CompanyData_Good[1:200,],CompanyData_Medium[1:200,])
CompanyData_test <- rbind(CompanyData_Bad[201:400,],CompanyData_Good[201:400,],CompanyData_Medium[201:400,])

CompanyData$ShelveLoc=as.numeric(CompanyData$ShelveLoc)
is.numeric(CompanyData$ShelveLoc)
CompanyData$Urban=as.numeric(CompanyData$Urban)
is.numeric(CompanyData$Urban)
CompanyData$US=as.numeric(CompanyData$US)
is.numeric(CompanyData$US)
cor(CompanyData)
boxplot(CompanyData)
mean(CompanyData$Price)
sd(CompanyData$Price)
CompPrice <- pnorm(CompanyData$Price,mean = 115.795,sd=23.67666, lower.tail=FALSE)
summary(CompanyData)

# Building a regression tree using rpart 
# Simple model
library(partykit)
model_CompanyData <- rpart(ShelveLoc~.,data=CompanyData,method="anova")
plot(model_CompanyData)
text(model_CompanyData)
model_CompanyData<-ctree(ShelveLoc~.,data=CompanyData)
plot(model_CompanyData)
text(model_CompanyData)
summary(model_CompanyData)
pred_Sales <- predict(model_CompanyData,CompanyData)
US_Sales <- sqrt(mean((pred_Sales-CompanyData$Sales)^2))
US_Sales

Adjusted_RSqred <- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

Adjusted_RSqred(pred_Sales,CompanyData$Sales) # 0.8484

plot(pred_Sales,CompanyData$Sales)
cor(pred_Sales,CompanyData$Sales) # 0.92
install.packages("party")
library(party)
