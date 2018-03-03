### R code from vignette source 'arulesViz.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: arulesViz.Rnw:75-78
###################################################
options(width = 75)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: arulesViz.Rnw:241-243
###################################################
library("arulesViz")
data("Groceries")


###################################################
### code chunk number 3: arulesViz.Rnw:253-254
###################################################
summary(Groceries)


###################################################
### code chunk number 4: arulesViz.Rnw:260-262
###################################################
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
rules


###################################################
### code chunk number 5: arulesViz.Rnw:270-271
###################################################
inspect(head(sort(rules, by ="lift"),3))


###################################################
### code chunk number 6: arulesViz.Rnw:281-283 (eval = FALSE)
###################################################
## plot(x, method = NULL, measure = "support", shading = "lift",
##   interactive = FALSE, data = NULL, control = NULL, engine = "default", ...)


###################################################
### code chunk number 7: scatterplot1
###################################################
plot(rules)


###################################################
### code chunk number 8: arulesViz.Rnw:344-345
###################################################
head(quality(rules))


###################################################
### code chunk number 9: scatterplot2
###################################################
plot(rules, measure=c("support", "lift"), shading="confidence")


###################################################
### code chunk number 10: scatterplot3
###################################################
plot(rules, method = "two-key plot")


###################################################
### code chunk number 11: arulesViz.Rnw:399-400 (eval = FALSE)
###################################################
## sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)


###################################################
### code chunk number 12: arulesViz.Rnw:477-479
###################################################
subrules <- rules[quality(rules)$confidence > 0.8]
subrules


###################################################
### code chunk number 13: matrix1
###################################################
plot(subrules, method="matrix", measure="lift")


###################################################
### code chunk number 14: matrix3D1
###################################################
plot(subrules, method="matrix3D", measure="lift")


###################################################
### code chunk number 15: matrix_col1
###################################################
plot(subrules, method="matrix", measure=c("lift", "confidence"))


###################################################
### code chunk number 16: clusterplot1
###################################################
plot(rules, method="grouped")


###################################################
### code chunk number 17: clusterplot2
###################################################
plot(rules, method="grouped", control=list(k=50))


###################################################
### code chunk number 18: arulesViz.Rnw:777-778 (eval = FALSE)
###################################################
## sel <- plot(rules, method="grouped", interactive=TRUE)


###################################################
### code chunk number 19: arulesViz.Rnw:797-798
###################################################
subrules2 <- head(sort(rules, by="lift"), 10)


###################################################
### code chunk number 20: graph1
###################################################
plot(subrules2, method="graph")


###################################################
### code chunk number 21: graph3
###################################################
plot(subrules2, method="graph", control=list(type="itemsets"))


###################################################
### code chunk number 22: arulesViz.Rnw:857-858 (eval = FALSE)
###################################################
## saveAsGraph(head(sort(rules, by="lift"),1000), file="rules.graphml")


###################################################
### code chunk number 23: pc1
###################################################
plot(subrules2, method="paracoord")


###################################################
### code chunk number 24: arulesViz.Rnw:922-924
###################################################
### for sampling
set.seed(4321)


###################################################
### code chunk number 25: pc2
###################################################
plot(subrules2, method="paracoord", control=list(reorder=TRUE))


###################################################
### code chunk number 26: arulesViz.Rnw:961-963
###################################################
oneRule <- sample(rules, 1)
inspect(oneRule)


###################################################
### code chunk number 27: doubledecker1
###################################################
plot(oneRule, method="doubledecker", data = Groceries)


