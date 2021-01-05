install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
install.packages("arules",repos = "http://cran.us.r-project.org")
install.packages("arulesViz",repos = "http://cran.us.r-project.org")
install.packages("arules")
install.packages("arulesViz")
library(RColorBrewer)
library(arules)
library(arulesViz)
library(tm)
detach(package:tm, unload=TRUE)
book <- read.csv(file.choose())
View(book)
str(book)
summary(book)

# Association Rules 
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.5,minlen=2))
rules
inspect(head(sort(rules, by = "lift")))
head(quality(rules))
windows()
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf[1:11])
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

redundent_rules<-is.redundant(rules)
redundent_rules
summary(redundent_rules)
rules<-rules[!redundent_rules]
rules
inspect(rules[1:11])
# count of each item from all books
# Visualizing rules in scatter plot
?plot
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "graph", interactive = T)

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
plot(oneRule, method = "doubledecker", data = book)

#Comparison of techniques
