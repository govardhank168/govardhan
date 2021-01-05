PSW <- read.csv("C:/Users/Johnny")
PSW = read.csv(file.choose())
View(PSW)

#Points
mean(PSW$Points)
median(PSW$Points)

getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(PSW$Points)

var(PSW$Points)
sd(PSW$Points)
range(PSW$Points)

#Score
mean(PSW$Score)
median(PSW$Score)

getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(PSW$Score)

var(PSW$Score)
sd(PSW$Score)
range(PSW$Score)

#Weigh
mean(PSW$Weigh)
median(PSW$Weigh)

getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(PSW$Weigh)

var(PSW$Weigh)
sd(PSW$Weigh)
range(PSW$Weigh)

#Expected value
a <- c(108, 110, 123, 134, 135, 145, 167, 187, 199)
mean(a)
sd(a)
integrand <- function(x){x*dlnorm(x,meanlog=145.3333)}
integrate(integrand,-Inf, Inf)

SD <- read.csv("C:/Users/Johnny/Desktop")
SD = read.csv(file.choose())
View(SD)

#Skewness
skewness(SD$speed)
skewness(SD$dist)
#kutosis
kurtosis(SD$speed)
kurtosis(SD$dist)

SW <- read.csv("C:/Users/Johnny/Desktop")
SW = read.csv(file.choose())
View(SW)

#Skewness
skewness(SW$SP)
skewness(SW$WT)
#kutosis
kurtosis(SW$SP)
kurtosis(SW$WT)


a <- c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
mean(a)
median(a)
var(a)
sd(a)
# to calculate Z score
qnorm(0.950)#90%
qnorm(0.970)#94%
qnorm(0.800)#60

#to calculate t score
qt(0.975,25)#0.95%
qt(0.980,25)#0.96%
qt(0.995,25)#0.99%

Ass21 <- read.csv("C:/Users/Johnny/desktop")
Ass21 = read.csv(file.choose())
getwd()
View(Ass21)

#Points
mean(Ass21$Measure.X)
median(Ass21$Measure.X)

a <- c(24.23, 25.53, 25.41, 24.14, 29.62, 28.25, 25.81, 24.39, 40.26, 32.95, 91.36, 25.99, 39.42, 26.71, 35.00)
mean(a/100)
sd(a/100)*sd(a/100)
boxplot(a)
p<- pnorm()-pnorm()