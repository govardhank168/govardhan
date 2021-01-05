#Question-1
getwd()
setwd("C:/Users/Johnny/Documents")

#Normality Test#

library(readxl)
Cutlets<-read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/Cutlets.xlsx")    # Cutlets.xlsx
attach(Cutlets)
colnames(Cutlets)<- c("Unit.A", "Unit.B")

#Changing Colnames#

View(Cutlets)
attach(Cutlets)
shapiro.test(Unit.A)
#p value= 0.32>0.05, p high Ho fly=> data are normal
shapiro.test(Unit.B)
#p value= 0.5225>0.05, p high Ho fly=> data are normal

var.test(Unit.A,Unit.B)
#p value= 0.3136>0.05, p high Ho fly=> Equal variance
t.test(Unit.A, Unit.B, alternative = "two.sided", conf.level = 0.095)
#p-value = 0.4723 > 0.05 accept null Hypothesis

#Question-2
LabTAT<-read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/LabTAT.xlsx")    # LabTAT.xlsx
attach(LabTAT)
colnames(LabTAT)<-c("Laboratory1","Laboratory2","Laboratory3","Laboratory4")
# Changing column names
View(LabTAT)
attach(LabTAT)

#Normality Test#
shapiro.test(Laboratory1)

#Question-3#
BuyerRatio<-read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/BuyerRatio.xlsx")
View(BuyerRatio)
attach(BuyerRatio)
table1 <- table(East, West, North, South)
table1
?prop.test
prop.test(x=c(20, 206),n=c(22,227),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#p-value = 2.2e-16 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 
prop.test(x=c(20, 206),n=c(22,227),conf.level = 0.95,correct = FALSE,alternative = "less")
#p-value = 0.5099 >0.05 accept null hypothesis

#Question-4#
CustomerOrderForm<-read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/CustomerOrderForm.xlsx")
View(CustomerOrderForm)
attach(CustomerOrderForm)
table1 <- table(Phillippines, Indonesia, Malta, India)
table1
?prop.test
prop.test(x=c(20, 206),n=c(22,227),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#p-value = 2.2e-16 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 
prop.test(x=c(20, 206),n=c(22,227),conf.level = 0.95,correct = FALSE,alternative = "less")
#Ho= propotion of defective < error free
#Ha= propotion of defective > error free
#p-value = 2.2e-16 < 0.05 accept null hypothes
#So dont process 
 
#quetion-5
Faltoons<-read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/Faltoons.xlsx")    # Cutlets.xlsx
View(Faltoons)
attach(Faltoons)
table1 <- table(Weekdays, Weekend)
table1
prop.test(x=c(167, 120),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#p-value = 0.2814371 > 0.05 accept null hypothesis
prop.test(x=c(66, 47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "less")
#p-value = 0.2814371 > 0.05 accept null hypothesis

#Anova Test#
library(readxl)
LabTAT <- read_excel("C:/Users/Johnny/Desktop/DS/Assignments/Hypotesting/LabTAT.xlsx")   
colnames(LabTAT) <- c("Laboratory1", "Laboratory2", "Laboratory3", "Laboratory4")
View(LabTAT)
attach(LabTAT)
shapiro.test(LabTAT$`Laboratory1`)
shapiro.test(LabTAT$`Laboratory2`)
shapiro.test(LabTAT$`Laboratory3`)
shapiro.test(LabTAT$`Laboratory4`)
summary(LabTAT)

# Data is normally distributed

var.test(Laboratory1,Laboratory2)
var.test(Laboratory2,Laboratory3)
var.test(Laboratory3,Laboratory4)

t.test(Laboratory1,Laboratory2,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
t.test(Laboratory2,Laboratory3,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
t.test(Laboratory3,Laboratory4,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
#p-value = 2.2e-16 > 0.05
#unequal mean so accept alternative hypothesis
t.test(Laboratory3,Laboratory2,alternative = "greater",var.equal = T)


library(car)
leveneTest(values~ ind, data = Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal 


# Customer order form 
# Unstacked data 

cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data 
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))


################# Mood's Median Test #################
install.packages("RVAideMemoire")
library(RVAideMemoire)
height <- read_excel(file.choose())
height
table(height$Treatment)

attach(height)
mood.medtest(Growth ~ Treatment,data = height,exact = FALSE)

############### Kruskal Wallis #################
kruskal.test(Growth,Treatment)

