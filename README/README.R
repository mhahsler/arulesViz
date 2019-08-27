library(arulesViz)
library(htmlwidgets)
library(plotly)
data(Groceries)

setwd("README")

unlink("js/", recursive = TRUE, force = TRUE)

rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
png("plot.png", width = 400, height = 360)
plot(rules)
dev.off()

p <- inspectDT(rules)
htmlwidgets::saveWidget(p, file="inspectDT.html", 
  selfcontained = FALSE, libdir = "js")

p <- plot(rules, engine = "plotly")
htmlwidgets::saveWidget(p, file="plotly_arules.html", selfcontained = FALSE, libdir = "js")

setwd("..")

