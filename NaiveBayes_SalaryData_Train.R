#setwd("D:\\ML\\R\\Naive Bayes")
install.packages("mlbench")
library(mlbench)
library(RColorBrewer)
SalaryData_Train<-read.csv(file.choose())

View(SalaryData_Train)
help("SalaryData_Train")
summary(SalaryData_Train)

barplot(table(as.factor(SalaryData_Train[,1]),as.factor(SalaryData_Train[,2])),legend=c("Federal-gov","Local-gov","Private","Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay"))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Federal-gov",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Local-gov",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Private",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Self-emp-inc",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Self-emp-not-inc",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="State-gov",2]))
plot(as.factor(SalaryData_Train[SalaryData_Train$workclass=="Without-pay",2]))
str(SalaryData_Train)
train_x<-SalaryData_Train[,c()]

# na values by column and class

na_by_col_class<-function(col,cls){
  return(sum(is.na(SalaryData_Train[,col]) & SalaryData_Train$Class==cls))
}

na_by_col_class("workclass","Federal-gov")
na_by_col_class("workclass","Local-gov")
na_by_col_class("workclass","Private")
na_by_col_class("workclass","Self-emp-inc")
na_by_col_class("workclass","Self-emp-not-inc")
na_by_col_class("workclass","Without-pay")

p_y_col_class<-function(col,class){
  sum_y<-sum(SalaryData_Train[,col]=="y" & SalaryData_Train$Class==class,na.rm = T)
  sum_n<-sum(SalaryData_Train[,col]=="n" & SalaryData_Train$Class==class,na.rm = T)
  return (sum_y/(sum_y+sum_n))
}
p_y_col_class(2,'Federal-gov')  
p_y_col_class(2,'Local-gov')
p_y_col_class(2,'Private')  
p_y_col_class(2,'Self-emp-inc')
p_y_col_class(2,'Self-emp-not-inc')  
p_y_col_class(2,'State-gov')
p_y_col_class(2,'Without-pay')  
na_by_col_class(2,'Federal-gov')
na_by_col_class(2,'Local-gov')
na_by_col_class(2,'Private')
na_by_col_class(2,'Self-emp-inc')
na_by_col_class(2,'Self-emp-not-inc')
na_by_col_class(2,'State-gov')
na_by_col_class(2,'Without-pay')

# imputing missing values 

set.seed(3)
train<-order(runif(290))
test<--train
training<-SalaryData_Train[train,]
testing<-SalaryData_Train[test,]
#train_X<-training[,-1]
#train_Y<-training[,1]
#test_X<-testing[,-1]
#test_Y<-testing[,1]

library(e1071)
model<-naiveBayes(training$workclass~.,data=training)
pred<-predict(model,newdata = testing[,-1])
mean(pred==testing[,1])

acc<-NULL


for (i in 1:20){train<-order(runif(350))
set.seed(350)
test<--train
training<-SalaryData_Train[train,]
testing<-SalaryData_Train[test,]
model<-naiveBayes(training$workclass~.,data=training[,-1])
pred<-predict(model,testing[,-1])
acc<-c(acc,mean(pred==testing[,1]))

}
acc



# Naive Bayes for the continuous data

data()
data("iris")
table(iris[,5])
View(iris3)
str(iris)
set.seed(135)
train<-order(runif(135))
t1<-train
t2<-train
test<--train
training<-iris[train,]
testing<-iris[test,]
table(training[,5])
library(e1071)
model_iris<-naiveBayes(training$Species~.,data = training[,-5])
pred_species<-predict(model_iris,testing[,-5])
mean(pred_species==testing[,5])
table(pred_species)
table(testing[,5])

boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
summary(iris$Petal.Length)
