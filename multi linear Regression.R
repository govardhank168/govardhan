Startups = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Multi Linear Regression/Startups.csv")
View(Startups)
attach(Startups)
plot(R.D.Spend, Profit)
summary(Startups)

#factor to numeric
as.numeric(as.factor(State))
Startups$State=as.numeric(Startups$State)
is.numeric(Startups$State)
attach(Startups)

plot(R.D.Spend, Profit)
plot(Marketing.Spend, Profit)
#correlation coefficient matrix - Strength & Direction of correlation
pairs(Startups)

##Partial Correlation - pure correlation between the variables
cor(Startups)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups))

#The Linear Model of interest
model.Startups <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State)
Summary(model.Startups)

model.StartupsR.D<-lm(Profit~R.D.Spend)
summary(model.StartupsR.D)

#question2

Computer_Data = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Multi Linear Regression/Computer_Data.csv")
View(Computer_Data)
attach(Computer_Data)
summary(Computer_Data)

#factor to numeric
as.numeric(as.factor(cd))
Computer_Data$cd=as.numeric(Computer_Data$cd)
is.numeric(Computer_Data$cd)

as.numeric(as.factor(multi))
Computer_Data$multi=as.numeric(Computer_Data$multi)
is.numeric(Computer_Data$multi)

as.numeric(as.factor(premium))
Computer_Data$premium=as.numeric(Computer_Data$premium)
is.numeric(Computer_Data$premium)
Computer_Data$X <- NULL #remove 'X' column
attach(Computer_Data)
cor(Computer_Data)
plot(ram, hd)
#correlation coefficient matrix - Strength & Direction of correlation
pairs(Computer_Data)

##Partial Correlation - pure correlation between the variables
cor(Computer_Data)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Computer_Data))

#The Linear Model of interest
model.Computer_Data <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
Summary(model.Computer_Data)

model.Computer_Data<-lm(ram~hd)
summary(model.Computer_Data)
reg <- lm(price~speed) #Linear regression
summary(reg)

model.Computer_Data<-lm(price~hd)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~ram)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~screen)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~cd)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~multi)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~premium)
summary(model.Computer_Data)
model.Computer_Data<-lm(price~ads)
summary(model.Computer_Data)

model.Computer_Data<-lm(price~trend)
summary(model.Computer_Data)

confint(reg, level=0.95)
predict(reg, interval="predict")
reg_log <- lm(ram ~ log(hd))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_exp <- lm(log(ram) ~ hd)
summary(reg_exp)
confint(reg, level=0.95)
predict(reg, interval="predict")

model.Computer_Data<-lm(trend~ads)
summary(model.Computer_Data)
reg <- lm(hd~ram) #Linear regression
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")

#Quetion3
ToyotaCorolla = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Multi Linear Regression/ToyotaCorolla.csv")
View(ToyotaCorolla)
attach(ToyotaCorolla)
summary(ToyotaCorolla)

#factor to numeric
as.numeric(as.factor(Model))
ToyotaCorolla$Model=as.numeric(ToyotaCorolla$Model)
is.numeric(ToyotaCorolla$Model)

ToyotaCorolla$Fuel_Type=as.numeric(ToyotaCorolla$Fuel_Type)
is.numeric(ToyotaCorolla$Fuel_Type)

ToyotaCorolla$Color=as.numeric(ToyotaCorolla$Color)
is.numeric(ToyotaCorolla$Color)
summary(ToyotaCorolla)
ToyotaCorolla$Id <- NULL
cor(ToyotaCorolla)
plot(Mfg_Year, Age_08_04)
#correlation coefficient matrix - Strength & Direction of correlation
pairs(ToyotaCorolla)

##Partial Correlation - pure correlation between the variables
cor(ToyotaCorolla)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(ToyotaCorolla))

#The Linear Model of interest
model.ToyotaCorolla<-lm(Mfg_Year~Age_08_04)
summary(model.ToyotaCorolla)
reg <- lm(Mfg_Year~Age_08_04) #Linear regression
summary(reg)

model.ToyotaCorolla<-lm(Mfg_Year~Price)
summary(model.ToyotaCorolla)
reg <- lm(Mfg_Year~Price) #Linear regression
summary(reg)
reg_log <- lm(Mfg_Year ~ log(Price))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")