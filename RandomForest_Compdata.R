install.packages("randomForest")
install.packages("C50") # we neeed to install C50 package to use ak
install.packages("tree")
install.packages("e1071")
install.packages("modeldata")
library(C50)
library(e1071)
library(modeldata)
library(randomForest)
library(ggplot2)
library(cowplot)
data <- read.csv(file.choose())
View(data)
summary(data)
data[data$ShelveLoc==0,]<-"Bad"
data[data$ShelveLoc==1,]<-"Good"
data[data$ShelveLoc==2,]<-"Medium"
data$ShelveLoc<-as.factor(data$ShelveLoc)
data[data$Urban==0,]<-"Yes"
data[data$Urban==1,]<-"No"
data$Urban<-as.factor(data$Urban)
data[data$US==0,]<-"Yes"
data[data$US==1,]<-"No"
data$US<-as.factor(data$US)

#data$ShelveLoc<-as.numeric(data$ShelveLoc)
#is.numeric(data$ShelveLoc)
#data$Urban<-as.numeric(data$Urban)
#is.numeric(data$Urban)
#data$US<-as.numeric(data$US)
#is.numeric(data$US)
str(data)
library(rpart)
model <- rpart(ShelveLoc~.,data=data,method="anova")
windows()
plot(model)
text(model)

#data.imputed<-rfImpute(ShelveLoc~., data=data, iter=6)
#Splitting data into training and testing. As the ShelveLoc are in order 
# splitting the data based on ShelveLoc
set.seed(123)
model_Good<-data[data$ShelveLoc=="Good",] # 200
model_Bad <- data[data$ShelveLoc=="Bad",] # 200
model_Medium <- data[data$ShelveLoc=="Medium",] # 200
View(data)
model_train <- rbind(model_Good[1:200,],model_Bad[1:200,],model_Medium[1:200,])
model_test <- rbind(model_Good[201:400,],model_Bad[201:400,],model_Medium[201:400,])

# Building a random forest model on training data
model<-randomForest(ShelveLoc~., data=data, ntree=600, proximity=TRUE)
mean(model$ShelveLoc==predict(model,model_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(model,model_train)
library(caret)
# Confusion Matrix
confusionMatrix(model_train$ShelveLoc, pred_train)
model
windows()
plot(model, lwd=2)
legend("topright",colnames(model$err.rate),col=1:3,cex=0.8,fill=1:3)

# Prediction of test data
pred_test <- predict(model,model_test)
# Confusion Matrix
confusionMatrix(model_test$ShelveLoc, pred_test)
model
windows()
plot(model, lwd=2)
legend("topright",colnames(model$err.rate),col=1:3,cex=0.8,fill=1:3)


norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to Company_data dataset
model <- as.data.frame(lapply(model[2:11], norm))

#statistics quest with ggplot
oob.values<-vector(length=10)
          for(i in 1:10) {
            temp.model<-randomForest(ShelveLoc~., data=data, mtry=i, ntree=1000)
            oob.values[i]<-temp.model$err.rate[nrow(temp.model$err.rate),1]
          }
oob.values
distance.matrix<-dist(1-model$proximity)
mds.stuff<-cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
          
mds.var.per<-round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
          
mds.values<-mds.stuff$points
mds.data<-data.frame(Sample=rownames(mds.values), X=mds.values[,1], Y=mds.values[,2], Status=data$ShelveLoc)
windows()
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
            geom_text(aes(color=Status))+theme_bw()+
            theme_bw()+
            xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
            ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +  
            ggtitle("MDS plot using (1 - Random Forest Proximities)")
#MDS - multi dimentional scalling

          