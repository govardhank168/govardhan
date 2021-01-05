install.packages("xlsx")
install.packages("xlsxjars")
install.packages("ggplot.multistats")
install.packages("ggplot2")
library(ggplot2)
library(ggplot.multistats)
library(readr)
library(XlsX)
library(XlsXjars)
library(rJava)
setwd("D:/DS/Assignments/Assignments/Forecasting")
#AirlinesData <- read.XlsX("D:/DS/Assignments/Assignments/Forecasting/Airlines+Data.XlsX") # read the Amtrack data
AirlinesData<-read.csv("D:/DS/Assignments/Assignments/Forecasting/Airlines+Data.csv")
View(AirlinesData) # Seasonality 12 X 
windows()
plot(AirlinesData$Passengers,type="o")
# So creating 12 dummy variables 

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 X
  View(X)

colnames(X)<-month.abb # Assigning month names 
View(X)
trakdata<-cbind(AirlinesData,X)
View(trakdata)
write.csv(trakdata,file = "trakdata.csv")
colnames(trakdata)[2]<-"Passenegers"
colnames(trakdata)
trakdata["t"]<- 1:96
View(trakdata)
trakdata["log_Passenger"]<-log(trakdata["Passenegers"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)

train<-trakdata[1:50,]

test<-trakdata[51:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passenegers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
write.csv(linear_pred,file = "linear_pred.csv")
rmse_linear<-sqrt(mean((test$Passenegers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 209.9256


######################### EXponential #################################

eXpo_model<-lm(log_Passenger~t,data=train)
summary(eXpo_model)
eXpo_pred<-data.frame(predict(eXpo_model,interval='predict',newdata=test))
rmse_eXpo<-sqrt(mean((test$Passenegers-eXpo_pred$fit)^2,na.rm = T))
rmse_eXpo # 217.0526


######################### Quadratic ####################################

Quad_model<-lm(Passenegers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passenegers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 137.15

######################### Additive Seasonality #########################

sea_add_model<-lm(Passenegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passenegers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 264.66

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passenegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passenegers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 168.6316

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passenegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passenegers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 50.60725

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(Passenegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passenegers-eXp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 268.197

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model<-lm(Passenegers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passenegers-multi_add_sea_pred$fit)^2,na.rm = T))
rmse_multi_add_sea # 172.76

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_eXpo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad"),c(rmse_linear,rmse_eXpo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
write.csv(table_rmse,file = "table_rmse.csv")

# Additive seasonality with Quadratic has least RMSE value

new_model <- lm(Passenegers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=trakdata)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.maX = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.maX = 15)
pred_res<- predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
windows()
acf(k$residuals)
write.csv(trakdata,file="trakdata.csv",col.names = F,row.names = F)
write.csv(table_rmse,file="table_rmse.csv",col.names = F,row.names = F)
write.csv(linear_pred,file="linear_pred.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
install.packages("readxl")
library(readXl)
test_data<-read.csv("D:/DS/Assignments/Assignments/Forecasting/Airlines+Data.csv")
View(test_data)
write.csv(test_data,file="test_data.csv")
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
write.csv(pred_new,file="pred_new.csv")
install.packages("forecast")
library(forecast)
pred_new$fit <- data.frame(pred_new$fit + pred_res$pred)
View(pred_new)
