##logistic regression
bank.full = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Logistic Regression/bank.full.csv")
summary(bank.full)
str(bank.full)
summary(bank.full)
bank.full$job=as.numeric(bank.full$job)
is.numeric(bank.full$job)
bank.full$marital=as.numeric(bank.full$marital)
is.numeric(bank.full$marital)
bank.full$education=as.numeric(bank.full$education)
is.numeric(bank.full$education)
bank.full$default=as.numeric(bank.full$default)
is.numeric(bank.full$default)
bank.full$housing=as.numeric(bank.full$housing)
is.numeric(bank.full$housing)
bank.full$loan=as.numeric(bank.full$loan)
is.numeric(bank.full$loan)
bank.full$contact=as.numeric(bank.full$contact)
is.numeric(bank.full$contact)
bank.full$month=as.numeric(bank.full$month)
is.numeric(bank.full$month)
bank.full$poutcome=as.numeric(bank.full$poutcome)
is.numeric(bank.full$poutcome)
bank.full$y=as.numeric(bank.full$y)
is.numeric(bank.full$y)

attach(bank.full)

summary(bank.full)
bank.full$y =as.factor(bank.full$y)

bank.full2 = na.omit(bank.full)


library(dummies)
dummies = dummy(bank.full$pdays , sep = "_")

# Imputation
library(Hmisc)
bank.full$age <- with(bank.full, impute(bank.full$age, mean))
summary(bank.full)


# Logistic Regression

logit=glm(y ~ age + factor(job) + factor(marital) + factor(education) + balance + factor(housing) + factor(loan) + duration + factor(poutcome),family= "binomial",data=bank.full)
?glm
summary(logit)


# Confusion Matrix Table

prob=predict(logit,type=c("response"),bank.full)
prob

confusion<-table(prob>0.5,bank.full$y)
confusion


# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
?diag
pred_values  = NULL
yes_NO = NULL
for (i in 1:45211){
  pred_values[i] = ifelse(prob[i]>= 0.5,1,0)
  yes_NO[i] = ifelse(prob[i]>= 0.5,'Yes',"no")
  
}
bank.full[,"prob"] = prob
bank.full[,"pred_values"] = pred_values
bank.full[,"yes_NO"] = yes_NO

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,bank.full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)
# More area under the ROC Curve better is the logistic regression model obtained

