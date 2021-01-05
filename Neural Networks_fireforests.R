forestfires <- read.csv(file.choose())
View(forestfires)
str(forestfires)
?str
forestfires$month=as.numeric(forestfires$month)
is.numeric(forestfires$month)
forestfires$day=as.numeric(forestfires$day)
is.numeric(forestfires$day)
forestfires$size_category=as.numeric(forestfires$size_category)
is.numeric(forestfires$size_category)
attach(forestfires)
forestfires_norm<-scale(forestfires)
## or 
?lapply
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires[,-31],FUN=normalize))
summary(forestfires_norm$month)
summary(forestfires_norm)
summary(forestfires$month)

forestfires_norm <- cbind(forestfires_norm,forestfires$month)
colnames(forestfires_norm)[31] <- "month"
forestfires_train<-forestfires_norm[1:260,]
forestfires_test<-forestfires_norm[261:517,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 
# Building model
formula_nn <- paste("month",paste(colnames(forestfires[-31],),collapse ="+"),sep="~")
forestfires_model <- neuralnet(month~day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep, data = forestfires_train)
forestfires_model <- neuralnet(formula = formula_nn,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(forestfires_model,forestfires_test[1:31])
str(model_results)
predicted_month <- model_results$net.result
# predicted_strength
# model_results$neurons
cor(predicted_month,forestfires_test$month)
plot(predicted_month,forestfires_test$month)
model_5<-neuralnet(month~day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep, data= forestfires_norm, hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,forestfires_test[1:31])
pred_month_5<-model_5_res$net.result
cor(pred_month_5,forestfires_test$month)
plot(pred_month_5,forestfires_test$month)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
write.csv(forestfires_norm, file="forestfires_norm.csv")
