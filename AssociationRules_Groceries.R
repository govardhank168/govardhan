install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
install.packages("arules",repos = "http://cran.us.r-project.org")
install.packages("arulesViz",repos = "http://cran.us.r-project.org")
install.packages("arules")
install.packages("arulesViz")
library(arulesViz)
search()
unloadNamespace("arules")
update.packages("arules") 
library(arules)
library(RColorBrewer)

Groceries <- read.csv(file.choose())
# Groceries1 <- readLines(file.choose()) # if we use readLines functions more lines will be reduced
View(Groceries)
str(Groceries)
# converting everything into character format 
Groceries[] <- lapply(Groceries,as.character)
View(Groceries)
# Creating a custom fucntion to collapse all the items in a transaction into a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
Groceries["new_col"] <- apply(Groceries,1,paste_fun)
View(Groceries)

install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
detach(package:tm, unload=TRUE)
Groceries_corpus <- Corpus(VectorSource(Groceries$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
Groceries_corpus <- tm_map(Groceries_corpus,stripWhitespace)
# Creating a TDM matrix
Groceries_dtm <- t(TermDocumentMatrix(Groceries_corpus))
# Converting TDM matrix to data frame
Groceries_dtm <- data.frame(as.matrix(Groceries_dtm))
View(Groceries_dtm)

# Association Rules 

library(arules)
library(arulesViz)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(Groceries_dtm,sum),col=1:189, main = "Item Frequecy plot")

# Association Rules 
# Applying apriori algorithm to get relevant rules
grocerie_rules <- apriori(as.matrix(Groceries_dtm),parameter = list(support=0.001,confidence=0.5,minlen=2))
grocerie_rules
detach(package:tm, unload=TRUE)
inspect(head(sort(grocerie_rules, by = "lift")))
head(quality(grocerie_rules))
plot(grocerie_rules)

# Sorting rules by confidence 
rules_conf <- sort(grocerie_rules,by="confidence")
inspect(rules_conf[1:189])
# Sorint rules by lift ratio
rules_lift <- sort(grocerie_rules,by="lift")
inspect(rules_lift)

redundent_rules<-is.redundant(grocerie_rules)
redundent_rules
summary(redundent_rules)
rules<-grocerie_rules[!redundent_rules]
rules
inspect(rules[1:189])
# count of each item from all the transactions 
# Visualizing rules in scatter plot
?plot
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "graph", interactive = T)
saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")

plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, method = "two-key plot")
sel <- plot(rules, measure=c("support", "lift"), shading = "confidence", interactive = TRUE)
subrules <- rules[quality(rules)$confidence > 0.8]
subrules
plot(subrules, method = "matrix", measure = "lift")
plot(subrules, method = "matrix3D", measure = "lift")
#Let's select 10 rules from subRules having the highest confidence.
top10subrules <- head(subrules, n = 10, by = "confidence")
plot(top10subrules, method = "graph",  engine = "htmlwidget")

#Grouped matrix-based visualization
windows()
plot(rules, method = "grouped")
#The group of most interesting rules according to lift (the default measure) are shown in the top-left corner of the plot. There are 3 rules
#which contain "Instant food products" and up to 2 other items in the antecedent and the
#consequent is "hamburger meat."
#To increase the number of groups we can change k which defaults to 20.
plot(rules, method = "grouped", control = list(k = 50))
#An interactive version of the grouped matrix visualization is also available
sel <- plot(rules, method = "grouped", interactive = TRUE)
#Here it is possible to zoom into groups and to inspect the rules contained in a selected group.

#Graph-based visualizations

subrules2 <- head(rules, n = 10, by = "lift")
install.packages("igraph")
install.packages("Rgraphviz",repos = "http://bioconductor.org/biocLite.R")
biocLite('Rgraphviz')
install.packages("Rgraphviz")
library(igraph)
library(Rgraphviz)
plot(subrules2, method = "graph")
saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")

#Parallel coordinates plot
plot(subrules2, method = "paracoord")
plot(subrules2, method = "paracoord", control = list(reorder = TRUE))

# Double Decker plots
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = Groceries)

#Comparison of techniques
