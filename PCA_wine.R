remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages("ggplot2")
install.packages('data.table', dependencies = TRUE)
install.packages(c("FactoMineR", "factoextra"))
install.packages("colorspace")
install.packages("crayon")
library(ellipse)
library(ellipsis)
library(colorspace)
library(crayon)
library("FactoMineR")
library("factoextra")
library(RColorBrewer)
#Data format
data<-read.csv(file.choose())
View(data)
head(data)
data.active <- data[1:178, 1:14]
head(data.active[, 1:14], 178)

#Data standardization
#When scaling variables, the data can be transformed as follow:
# xi???mean(x)/sd(x) Where mean(x) is the mean of x values, and sd(x) is the standard deviation (SD).
windows()
install.packages("digest")
library(digest)
PCA(data.active, scale.unit = TRUE, ncp = 5, graph = TRUE)

#The R code below, computes principal component analysis on the active individuals/variables:
library("FactoMineR")
wine.pca <- PCA(data.active, graph = FALSE)

#The output of the function PCA() is a list, including the following components :
print(wine.pca)

#Visualization and Interpretation
#1.Eigenvalues / Variances
library("factoextra")
eig.val <- get_eigenvalue(wine.pca)
eig.val

#The scree plot can be produced using the function fviz_eig() or fviz_screeplot() [factoextra package].
fviz_eig(wine.pca, addlabels = TRUE,  ylim = c(0, 50),col=brewer.pal(n = 10, name = "RdBu"))

#Graph of variables
#Results:A simple method to extract the results, for variables, from a PCA output is to use the function get_pca_var() [factoextra package]. This function provides a list of matrices containing all the results for the active variables (coordinates, correlation between variables and axes, squared cosine and contributions)

var <- get_pca_var(wine.pca)
var

#The different components can be accessed as follow:

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#Correlation circle
# Coordinates of variables
head(var$coord, 14)

#To plot variables, type this:
fviz_pca_var(wine.pca, col.var = "red")

#Quality of representation
head(var$cos2, 14)


#We can visualize the cos2 of variables on all the dimensions using the corrplot package:
library("corrplot")
corrplot(var$cos2, is.corr=FALSE, col = c(n = 10, name = rainbow(n=10))) #It's also possible to create a bar plot of variables cos2 using the function fviz_cos2()[in factoextra]:

# Total cos2 of variables on Dim.1 to Dim.5
fviz_cos2(wine.pca, choice = "var", axes = 1:5)

# Color by cos2 values: quality on the factor map
fviz_pca_var(wine.pca, col.var = "cos2",
             gradient.cols = brewer.pal(n = 11, name = "RdBu"), 
             repel = TRUE
) #repel = TRUE - Avoid text overlapping

# Change the transparency by cos2 values
fviz_pca_var(wine.pca, alpha.var = "cos2",gradient.cols = brewer.pal(n = 11, name = "RdBu"))

#Contributions of variables to PCs
head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE,col = c(n = 10, name = rainbow(n=10)))

# Contributions of variables to PC1
fviz_contrib(wine.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(wine.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(wine.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(wine.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(wine.pca, choice = "var", axes = 5, top = 10)

#The total contribution to PC1 and PC2 is obtained with the following R code:
fviz_contrib(wine.pca, choice = "var", axes = 1:2, top = 10)  

#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(wine.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Change the transparency by contrib values
fviz_pca_var(wine.pca, alpha.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Color by a custom continuous variable
# Create a random continuous variable of length 10
set.seed(123)
my.cont.var <- rnorm(10)
# Color variables by the continuous variable
fviz_pca_var(wine.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cont.Var")
#Color by groups
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(wine.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

res.desc <- dimdesc(wine.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2

#Graph of individuals
ind <- get_pca_ind(wine.pca)
ind

#To get access to the different components, use this:
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

#Plots: quality and contribution
fviz_pca_ind(wine.pca)

#Like variables, it's also possible to color individuals by their cos2 values:
fviz_pca_ind(wine.pca, col.ind = "cos2", 
             gradient.cols = c(n = 10, name = rainbow(n=10)),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#We can also change the point size according the cos2 of the corresponding individuals:

fviz_pca_ind(wine.pca, pointsize = "cos2", 
             pointshape = 21, fill = "red",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

#To change both point size and color by cos2, try this:
fviz_pca_ind(wine.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c(n = 10, name = rainbow(n=10)),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_cos2(wine.pca, choice = "ind")

#To visualize the contribution of individuals to the first two principal components, type this:

# Total contribution on PC1 and PC2
fviz_contrib(wine.pca, choice = "ind", axes = 1:2)

#Color by a custom continuous variable
# Create a random continuous variable of length 23,
# Same length as the number of active individuals in the PCA
set.seed(123)
my.cont.var <- rnorm(23)
# Color individuals by the continuous variable

my.cont.var
fviz_pca_ind(wine.pca, col.ind = "cos2",
             gradient.cols = c(n = 10, name = rainbow(n=10)),
             legend.title = "Cont.Var", repel = TRUE,
             )

#Summary: 
#1.Using prcomp() [stats]
res.pca <- prcomp(ind$coord[,-14], scale. = TRUE)
#2.Using princomp() [stats]
res.pca <- princomp(ind$coord[,-14], cor = TRUE)
#3.Using dudi.pca() [ade4]
install.packages("ade4")
library("ade4")
res.pca <- dudi.pca(ind$coord[,-14], scannf = FALSE, nf = 5)
#4.Using epPCA() [ExPosition]
library("ExPosition")
res.pca <- epPCA(ind$coord[, -14], graph = FALSE)

fviz_eig(res.pca)     # Scree plot
fviz_pca_ind(res.pca) # Graph of individuals
fviz_pca_var(res.pca) # Graph of variables
