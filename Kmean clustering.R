EastWestAirlines <- read.csv(file.choose())
View(EastWestAirlines)

install.packages("plyr")
library(plyr)
x<- runif(500)

y<- runif(500)

data <- cbind(x,y)
plot(data)
plot(data, type="n")
text(data, rownames(data))

km <- kmeans(data, 6)
str(km)
install.packages("animation")
library(animation)

km1 <- kmeans.ani(data, 6)
km$centers
km$cluster


EastWestAirlines = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
for (i in 2:11) EastWestAirlines[i] = sum(kmeans(normalized_data, centers = i)$withinss)
plot(2:11, EastWestAirlines, type = "b", xlab = "Numbers of Clusters", ylab = "Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")

install.packages("kselection")
library(kselection)

install.packages("doparallel")
library(doParallel)
registerDoParallel(cores=4)
?kselection
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
normalized_data<-scale(EastWestAirlines[,2:11]) #excluding the university name columnbefore normalizing
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
groups <- cutree(fit, k=6)
mileageoffers<-as.matrix(groups)
final <- data.frame(EastWestAirlines, mileageoffers)
View(final)
write.csv(final, file="final.csv",row.names = F)

aggregate(EastWestAirlines[,-1],by=list(final$mileageoffers),mean)
#EastWestAirlines <- EastWestAirlines[1:25, c(1,3:8)]
#normalized_data <- scale(mydata[,2:11])
#fit <- kmeans(normalized_data, 5)
#final2 <- data.frame(mydata, fit$Cluster)
#final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
#aggregate(mydata[,2:11], by=list(fit$cluster), FUN=mean)

install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(3999, 0 , 8), rnorm(3999, 0, 8)),cbind(rnorm(3999, 0 , 8), rnorm(3999, 0, 8)))
xcl <- clara(xds,2, samples = 1000)
clusplot(xcl)

xam <- pam(xds, 6)

normalized_data<-scale(EastWestAirlines[,2:5]) #excluding the university name columnbefore normalizing
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
final <- data.frame(EastWestAirlines, clusters)
View(final)
write.csv(final, file="final.csv",row.names = F)

aggregate(EastWestAirlines[,-1],by=list(final$clusters),mean)
