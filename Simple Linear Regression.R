#Quetion1
calories_consumed = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Simple Linear Regression/calories_consumed.csv")
View(calories_consumed)
attach(calories_consumed)
plot(Weight.gained..grams., Calories.Consumed)
?regression
reg <- lm(Weight.gained..grams.~ Calories.Consumed) #Linear regression
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")

#Quetion2
delivery_time = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Simple Linear Regression/delivery_time.csv")
View(delivery_time)
attach(delivery_time)
plot(Delivery.Time, Sorting.Time)
reg <- lm(Delivery.Time ~ Sorting.Time) #Linear regression
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_log <- lm(Sorting.Time ~ log(Delivery.Time))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_exp <- lm(log(Sorting.Time) ~ Delivery.Time)
summary(reg_exp)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_rec <- lm(exp(Sorting.Time) ~ Delivery.Time)
summary(reg_rec)
confint(reg, level=0.95)
predict(reg, interval="predict")

#Quetion3
emp_data = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Simple Linear Regression/emp_data.csv")
View(emp_data)
attach(emp_data)
plot(Salary_hike, Churn_out_rate)
reg <- lm(Salary_hike ~ Churn_out_rate) #Linear regression
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_log <- lm(Churn_out_rate ~ log(Salary_hike))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")

#Quetion 4
Salary_Data = read.csv("C:/Users/Johnny/Desktop/DS/Assignments/Simple Linear Regression/Salary_Data.csv")
View(Salary_Data)
attach(Salary_Data)
plot(YearsExperience, Salary)
reg <- lm(YearsExperience ~ Salary) #Linear regression
summary(reg)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_log <- lm(YearsExperience ~ log(Salary))
summary(reg_log)
confint(reg, level=0.95)
predict(reg, interval="predict")


reg_exp <- lm(log(YearsExperience) ~ Salary)
summary(reg_exp)
confint(reg, level=0.95)
predict(reg, interval="predict")
