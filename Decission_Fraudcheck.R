install.packages("party")
library(party)
library(tree)
library(FSelector)
library(rpart)
library(caret)
library(dplyr)
library(rpart.plot)
library(tree)
library(caTools)
library(ElemStatLearn)
Fraudcheck <- read.csv(file.choose())
print(head(Fraudcheck))
# Create the input data frame.
Fraudcheck <- readingSkills[c(1:401),]

# Give the chart file a name.
png(file = "decision_tree.png")

# Create the tree.
High = ifelse(Fraudcheck$Taxable.Income<=30000, "Risky", "Good")
Fraudcheck = data.frame(Fraudcheck, High)
output.tree <- ctree(
  High ~ ., 
  data = Fraudcheck)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()

Fraudcheck <- read.csv(file.choose())
names(Fraudcheck)
windows()
hist(Fraudcheck$Taxable.Income,main = "Taxable Income",xlim = c(0,97000),
     breaks=c(seq(20,30,40)), col = c("blue","red", "green","violet"))
High = ifelse(Fraudcheck$Taxable.Income<=30000, "Risky", "Good")
Fraudcheck = data.frame(Fraudcheck, High)

#Now let's fill a model using decision trees. Of course, you can't have the Sales variable here because your response variable High was created from Sales. Thus, let's exclude it and fit the tree.

tree.Fraudcheck = tree(High~., data=Fraudcheck)
summary(tree.Fraudcheck)

#You can see the variables involved, the number of terminal nodes, the residual mean deviance, as well as the misclassification error rate. To make it more visual, let's plot the tree as well, then annotate it using the handy text function:
windows()
plot(tree.Fraudcheck)
text(tree.Fraudcheck, pretty = 0)

#There are so many variables, making it very complicated to look at the tree. At least, you can see that at each of the terminal nodes, they're labeled Yes or No. At each splitting node, the variables and the value of the splitting choice are shown (for example, Price < 92.5 or Advertising < 13.5).

#For a detailed summary of the tree, simply print it. It'll be handy if you want to extact details from the tree for other purposes:

tree.Fraudcheck

#It's time to prune the tree down. Let's create a training set and a test by splitting the Fraudcheck dataframe into 250 training and 150 test samples. First, you set a seed to make the results reproducible. Then you take a random sample of the ID (index) numbers of the samples. Specifically here, you sample from the set 1 to n row number of rows of car seats, which is 400. You want a sample of size 250 (by default, sample uses without replacement).

set.seed(101)
train=sample(1:nrow(Fraudcheck), 250)

#So now you get this index of train, which indexes 250 of the 400 observations. You can refit the model with tree, using the same formula except telling the tree to use a subset equals train. Then let's make a plot:

tree.Fraudcheck = tree(High~., Fraudcheck, subset=train)
plot(tree.Fraudcheck)
text(tree.Fraudcheck, pretty=0)

tree.pred = predict(tree.Fraudcheck, Fraudcheck[-train,], type="class")

with(Fraudcheck[-train,], table(tree.pred, High))

cv.Fraudcheck = cv.tree(tree.Fraudcheck, FUN = prune.misclass)
cv.Fraudcheck

#Printing out the results shows the details of the path of the cross-validation. You can see the sizes of the trees as they were pruned back, the deviances as the pruning proceeded, as well as the cost complexity parameter used in the process.

plot(cv.Fraudcheck)

#Looking at the plot, you see a downward spiral part because of the misclassification error on 250 cross-validated points. So let's pick a value in the downward steps (12). Then, let's prune the tree to a size of 12 to identify that tree. Finally, let's plot and annotate that tree to see the outcome.

prune.Fraudcheck = prune.misclass(tree.Fraudcheck, best = 12)
plot(prune.Fraudcheck)
text(prune.Fraudcheck, pretty=0)

#It's a bit shallower than previous trees, and you can actually read the labels. Let's evaluate it on the test dataset again.

tree.pred = predict(prune.Fraudcheck, Fraudcheck[-train,], type="class")
with(Fraudcheck[-train,], table(tree.pred, High))

#Random Forests
library(randomForest)
Fraudcheck <- read.csv(file.choose())
names(Fraudcheck)

set.seed(101)
train = sample(1:nrow(Fraudcheck), 300)

rf.Fraudcheck = randomForest(High~., data = Fraudcheck, subset = train)
rf.Fraudcheck

oob.err = double(13)
test.err = double(13)
for(mtry in 1:13){
  fit = randomForest(High~., data = Fraudcheck, subset=train, mtry=mtry, ntree = 100)
  oob.err[mtry] = fit$mse[100]
  pred = predict(fit, Fraudcheck[-train,])
  test.err[mtry] = with(Fraudcheck[-train,], mean( (High-pred)^2 ))
}

#Boosting
library(gbm)
library(ggplot2)


boost.Fraudcheck = gbm(High~., data = Fraudcheck[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.Fraudcheck)

plot(boost.Fraudcheck,mtry="lstat")
plot(boost.Fraudcheck,mtry="rm")

n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.Fraudcheck, newdata = Fraudcheck[-train,], n.trees = n.trees)
dim(predmat)

boost.err = with(Fraudcheck[-train,], apply( (predmat - Taxable.Income)^2, 2, mean) )
plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")
abline(h = min(test.err), col = "red")
