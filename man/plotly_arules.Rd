\name{plotly_arules}
\Rdversion{1.1}
\alias{plot}
\alias{plotly}
\alias{plotly_arules}
\title{Interactive Scatter Plot for Association Rules using plotly}
\description{
Plot an interactive scatter plot for association rules using \pkg{plotly}.
}
\usage{
plotly_arules(x, measure = c("support", "confidence"), 
  shading = "lift", max = 1000, colors = colorRamp(c("grey", "red")),
  size=7, opacity = .8, ...)
}
\arguments{
  \item{x}{ an object of class "rules". }
  \item{measure}{ measure(s) of interestingness 
  (e.g., "support", "confidence", "lift", "order") used in the visualization as x
  and y-axis.}
  \item{shading}{ measure of interestingness used for color shading.}
  \item{max}{ client side processing in plotly is expensive. We restrict the 
  number of rules to the max best rules (according to the measure used for shading.)}
  \item{colors}{  color palette used.}
  \item{size}{  marker size.}
  \item{opacity}{ opacity of markers.}
  \item{\dots}{ further arguments are passed on to \code{plot_ly()}.}
}
\value{
The plotly object (plotly_hash).
}
%\details{}
%\references{}
%\seealso{}
\examples{
\dontrun{  
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
rules

plotly_arules(rules)
plotly_arules(rules, measure = c("support", "lift"), shading = "order")

### add a title
plotly_arules(rules) %>% layout(title = "Some Grocery Rules")

### save a plot as a html page
p <- plotly_arules(rules)
htmlwidgets::saveWidget(as.widget(p), "arules.html")}
}
\keyword{ hplot }
