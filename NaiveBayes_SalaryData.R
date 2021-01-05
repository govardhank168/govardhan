install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
install.packages('psych')
library(psych)
install.packages('Amelia')
library(Amelia)
install.packages('mice')
library(mice)
install.packages('GGally')
library(GGally)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)

#Step 2: Import the data set
data<-read.csv(file.choose())
View(data)
#Setting outcome variables as categorical
#data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))

#Studying the structure of the data

str(data)

#Understanding the data set 
head(data)
#Understanding the data set - Naive Bayes 

describe(data)

#Convert '0' values into NA
data[, 1:14][data[, 1:14] == 0] <- NA

#visualize the missing data
windows()
missmap(data)

#Use mice package to predict missing values
mice_mod <- mice(data[, c("age",	"workclass",	"education",	"educationno",	"maritalstatus",	"occupation",	"relationship",	"race",	"sex",	"capitalgain",	"capitalloss",	"hoursperweek",	"native",	"Salary")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the main data set
data$capitalgain <- mice_complete$capitalgain
data$capitalloss <- mice_complete$capitalloss

#To check if there are still any missing values, let's use the missmap plot:
missmap(data)
cardinality_threshold=17
data %>% select(-education)
#Data Visualization
#Visual 1
ggplot(data, aes(age, colour = Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Salary")

#visual 2
c <- ggplot(data, aes(x=capitalloss, fill=capitalloss, color=Salary)) +
  geom_histogram(binwidth = 1) + labs(title="workclass Distribution by Salary")
c + theme_bw()

#visual 3
P <- ggplot(data, aes(x=capitalgain, fill=capitalgain, color=Salary)) +
  geom_histogram(binwidth = 1) + labs(title="capitalgain Distribution by Salary")
P + theme_bw()

#visual 4
ggplot(data, aes(hoursperweek, colour = Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="hoursperweek Distribution by Salary")

#visual 5
cardinality_threshold=16
pairs.panels(data)
library(ggplot2)
require(GGally)
ggpairs(data)

#split data into training and test data sets
indxTrain <- createDataPartition(y = data$Salary,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,] #Check dimensions of the split > prop.table(table(data$Outcome)) * 100

prop.table(table(training$Salary)) * 100

prop.table(table(testing$Salary)) * 100

#create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9]
y = training$Salary

#Now it's time to load the e1071 package that holds the Naive Bayes function. This is an in-built function provided by R.
library(e1071)

#After loading the package, the below code snippet will create Naive Bayes model by using the training data set:

install.packages("klaR", version("mice"))
update.packages("klaR")
install.packages("scales")
install.packages('lazyeval')
install.packages("pkgconfig")
library('tidyverse')
library(pkgconfig)
library(lazyeval)
library(klaR)
#model<-naiveBayes(Salary~., data = data)
model = naiveBayes(x,y,'nb',trControl=trainControl(method='cv',number=10))
model
plot(model)
#Model Evaluation
#Predict testing set
Predict <- predict(model,newdata = testing ) #Get the confusion matrix to see accuracy value and other parameter values > confusionMatrix(Predict, testing$Outcome )

#Confusion Matrix and Statistics
#Plot Variable performance
install.packages("varImp")
library(varImp)
X <- varImp(model)
plot(X)

