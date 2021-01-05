Startups <- read.csv(file.choose())
View(Startups)
str(Startups)
?str
Startups$State=as.numeric(Startups$State)
is.numeric(Startups$State)
attach(Startups)
Startups_norm<-scale(Startups)
## or 
?lapply
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups[,-6],FUN=normalize))
summary(Startups_norm$Profit)
summary(Startups_norm)
summary(Startups$Profit)

Startups_norm <- cbind(Startups_norm,Startups$Profit)
colnames(Startups_norm)[5] <- "Profit"
Startups_train<-Startups_norm[1:25,]
Startups_test<-Startups_norm[26:50,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("Profit",paste(colnames(Startups[-5]),collapse ="+"),sep="~")
Startups_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = Startups_train)
Startups_model <- neuralnet(formula = formula_nn,data = Startups_train)
str(Startups_model)
plot(Startups_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(Startups_model,Startups_test[1:5])
str(model_results)
predicted_Profit <- model_results$net.result
# predicted_Profit
# model_results$neurons
cor(predicted_Profit,Startups_test$Profit)
plot(predicted_Profit,Startups_test$Profit)
Startups_model<-neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State, data= Startups_norm, hidden = 5)
View(Startups_model)
plot(Startups_model)
Startups_model_res<-compute(Startups_model,Startups_test[1:5])
pred_strn_5<-Startups_model_res$net.result
cor(pred_strn_5,Startups_test$Profit)
plot(pred_strn_5,Startups_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
write.csv(Startups_norm, file="Startups_norm.csv")
