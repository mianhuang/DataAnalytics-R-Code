# install a package for analyzing transactional data
install.packages("arules")

# install package that provides visual techniques for the arules package
install.packages("arulesViz")
install.packages("caTools")
install.packages("prabclus")
install.packages("DEoptimR")
install.packages("trimcluster")

# calling libraries
library(arules)
library(arulesViz)

options(digits = 2)
set.seed(1234)

# upload dataset
electronidex <-read.transactions("ElectronidexTransactions2017.csv", 
                                 format = "basket", header=FALSE, sep=",", 
                                 cols=NULL, rm.duplicates=FALSE, skip=0)
summary(electronidex)

# review dataset
inspect(electronidex[1:10])
length(electronidex)
size(electronidex)
LIST(electronidex)
itemLabels(electronidex)

# visualize dataset
itemFrequencyPlot(electronidex, topN=20)
image(sample(electronidex, 100))
# sparse matrix for the first 20 transactions
image(electronidex[1:20])


# apply the Apriori algorithm to find association rules
rules <- apriori (electronidex, parameter = list(supp = 0.01, conf = 0.5, minlen=2))
summary(rules)

args(getS3method("plot", "rules"))

# sort rules by support, confidence or lift measures
inspect(sort(rules, by="lift"))
inspect(head(sort(rules, by="lift"),3))
inspect(rules[1:10])

# Visualize rules with scatter plot
plot(rules)
head(quality(rules))
plot(rules, measure=c("support","lift"), shading="confidence")
plot(rules, shading="order", method = "two-key plot")
sel <- plot(rules, measure=c("support", "lift"), shading = "confidence",
            method = "two-key plot", interactive = TRUE)

# filter rules for >0.8 confidence
subrules <- rules[quality(rules)$confidence > 0.8]
subrules
# view rules in matrix plot
plot(subrules, method = "matrix", measure = "lift")
plot(subrules, method = "matrix3D", measure = "lift")

#view matrix visualization with grouped antecedents
sel<-plot(rules, method = "grouped", control = list(k = 50), interactive = TRUE)

# select the 10 rules with the highest lift
subrules2 <- head(rules, n = 10, by = "lift")
# Graph based visualization
plot(subrules2, method = "graph")
plot(subrules2, method = "paracoord", control = list(reorder = TRUE))

# write rules to a csv file
write(rules, file = "rules.csv", sep = ",")
# convert the rules into an R data frame
rules_df <- as(rules, "data.frame")
View(rules_df)

#view specific rules
inspect(rules[10], ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", 
        +         linebreak = FALSE)
# view rules with specific items
iMacRules <- subset(rules, items %in% "iMac")
inspect(iMacRules)

# check for redundant rules
is.redundant(rules) 
# Pruning redundant rules
rules.pruned <- rules[!is.redundant(rules)]
inspect(rules.pruned)
write(rules.pruned, file = "pruned_rules.csv", sep = ",")

# a different method for pruning redundant rules
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned2 <- rules[!redundant]
summary(rules.pruned2)


