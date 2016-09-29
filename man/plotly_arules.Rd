\name{plotly_arules}
\Rdversion{1.1}
\alias{plotly}
\alias{plotly_arules}
\title{Interactive Scatter Plot for Association Rules using plotly}
\description{
Plot an interactive scatter plot for association rules using \pkg{plotly}.
}
\usage{
plotly_arules(x, method = "scatterplot", measure = c("support", "confidence"), 
  shading = "lift", max = 1000, ...)
}
\arguments{
  \item{x}{ an object of class "rules". }
  \item{method}{ currently the methods "scatterplot" and "matrix" are supported.}
  \item{measure}{ measure(s) of interestingness 
  (e.g., "support", "confidence", "lift", "order") used in the visualization as x
  and y-axis.}
  \item{shading}{ measure of interestingness used for color shading.}
  \item{max}{ client side processing in plotly is expensive. We restrict the 
  number of rules to the max best rules (according to the measure used for shading.)}
  \item{\dots}{ colors can be used to specify a color palette. 
    Further arguments are passed on to \code{plot_ly()} as
    markter attributes (e.g., size, symbol and opacity).}
}
\value{
The plotly object (plotly_hash).
}
%\details{}
%\references{}
%\seealso{}
\examples{
\dontrun{ 
library(plotly)
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
rules

### interactive scatter plot visualization
plotly_arules(rules)
plotly_arules(rules, measure = c("support", "lift"), shading = "order")

# add a title
plotly_arules(rules) %>% layout(title = "Some Grocery Rules")

# save a plot as a html page
p <- plotly_arules(rules)
htmlwidgets::saveWidget(as.widget(p), "arules.html")

### Interactive matrix visualization
plotly_arules(rules, method = "matrix") %>% layout(title = "Reordered Matrix")
  
}
}
\keyword{ hplot }
