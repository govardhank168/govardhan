install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('ggplot2')  #for Data Visualization
install.packages('class')    #KNN 
install.packages('caret')    #Confusion MatTypex
install.packages('corrplot') #Correlation Plot
install.packages("kknn")
library(kknn)
library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)

# Read the dataset
glass <- read.csv(file.choose())
View(glass)
#First colum in dataset is id which is not required so we will be taking out
#glass <- glass[-1]

#table of diagonis B <- 357 and M <- 212
table(glass$Type)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
glass$Type <- factor(glass$Type, levels = c("1","2","3","4","5","6","7"), labels = c("building_windows_float_processed","building_windows_non_float_processed","vehicle_windows_float_processed","vehicle_windows_non_float_processed","containers","tableware","headlamps"))
table(glass$Type)
prop.table(table(glass$Type))
round(prop.table(table(glass$Type))*100, digits=1)
# table or proportation of enteTypees in the datasets. What % of entry is Bengin and % of entry is Malignant
#round(prop.table(table(glass$Type))*100,1)
summary(glass[c("radius_mean","texture_mean","perimeter_mean")])

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to glass dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
round(prop.table(table(glass$Type))*100, digits=1)
View(glass_n)
#create training and test datasets
glass_train <- glass_n[1:50,]
glass_test <- glass_n[51:101,]

#Get labels for training and test datasets

glass_train_labels <- glass_n[1:50,1]
glass_test_labels <- glass_n[51:101,1]

# Build a KNN model on taining dataset
library(class)
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
predicted.type <- knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=5)
#------Evoluting model performance----#
#---load gmodels--#
install.packages("gmodels")
library(gmodels)
#create cross tablulation predicted vs actual
CrossTable(x=glass_test_labels, y=predicted.type,prop.chisq = FALSE)
#improving model performance#
#use scale() function to z-score standerdize data frame
glass_z<-as.data.frame(scale(glass_test_labels[-1]))
#Error in prediction
error <- mean(predicted.type!=glass_test_labels)
#Confusion Matrix
confusionMatrix(as.character(glass$Type), predicted.type)
test_Type <- NULL
train_Type <- NULL
for (i in seq(3,101,2)){
  train_glass_pred <- knn(train = glass_train,test = glass_test,cl = glass_train_labels,k=8)
  train_Type <- c(train_Type,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=9)
  test_Type <- c(test_Type,mean(test_glass_pred==glass_test_labels))
}

i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=i)
  k.optm[i] <- 100 * sum(glass_test_labels == knn.mod)/NROW(glass_test_labels)
  k=i
  cat(k,'=',k.optm[i],'')}
install.packages('class')
library(class)

train_acc <- 100*sum(glass_train_labels == 7)/NROW(glass_train_labels)
head(train_acc)
test_acc <- 100*sum(glass_test_labels == 7)/NROW(glass_test_labels)
# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
windows()
plot(seq(0,1,2), train_acc, type="l", main="Train_acc", col="blue")
plot(seq(10,20,30), test_acc, type="l",main="Test_acc",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,100,2)))
acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,100,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)

