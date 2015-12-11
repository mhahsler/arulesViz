\name{plot}
\Rdversion{1.1}
\alias{plot}
\alias{plot.itemsets}
\alias{plot.rules}
\alias{plot.grouped_matrix}
\title{Plot method to visualize association rules and itemsets}
\description{
This is the S3 method to visualize association rules and itemsets.
Implemented are several popular visualization methods 
including scatter plots with shading (two-key plots), 
graph based visualizations, doubledecker plots, etc.
}
\usage{
\method{plot}{rules}(x, method = NULL, measure = "support", shading = "lift", 
    interactive = FALSE, data = NULL, control = NULL, ...)
\method{plot}{itemsets}(x, method = NULL, measure = "support", shading = NA,
    interactive=FALSE, data = NULL, control = NULL, ...)
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets". }
  \item{method}{ a string with value "scatterplot", "two-key plot", "matrix", 
  "matrix3D",  "mosaic", 
  "doubledecker", "graph", "paracoord" or "grouped", "iplots" selecting the 
  visualization method (see Details).}
  
  \item{measure}{ measure(s) of interestingness 
  (e.g., "support", "confidence", "lift", "order") used in the visualization. Some 
  visualization methods need one measure, others take a vector with two 
  measures (e.g., scatterplot). In some plots (e.g., graphs) \code{NA} 
  can be used to suppress using a measure. }
  
  \item{shading}{ measure of interestingness used
  for the color of the points/arrows/nodes
  (e.g., "support", "confidence", "lift"). The default is "lift".
  \code{NA} can be often used to suppress shading.}
  
  \item{interactive}{ enable interactive exploration (not implemented by 
  all methods).}
  
  \item{control}{ a list of control parameters for the plot. The available
  control parameters depends on the visualization technique (see Details).}
  
  \item{data}{  the dataset (class "transactions") 
  used to generate the rules/itemsets. Only 
  "mosaic" and "doubledecker" require the original data.}
  
  \item{\dots}{ further arguments are typically passed on to the 
  used low-level plotting function. }
}
\details{
Most visualization techniques are described by Bruzzese and Davino (2008),
however, we added more color shading, reordering and interactive features.
The following visualization method are available:

\describe{
\item{"scatterplot", "two-key plot"}{ 
This visualization method draws a two dimensional scatterplot with different
measures of interestingness (parameter "measure") on the axes and a third 
measure (parameter "shading") is represented by the color of the points. 
There is a special value for shading called "order" which produces a
two-key plot where the color of the points represents the length (order) 
of the rule.

The list of control parameters for this method is 
\describe{
\item{"main"}{plot title}
\item{"pch"}{ use filled symbols: 20--25}
\item{"cex"}{symbol size}
\item{"xlim","ylim"}{limits}
\item{"jitter"}{a number greater than 0 adds jitter to the points}
\item{"col"}{color palette}
}

Interactive manipulations are available.
}

\item{"matrix", "matrix3D"}{ 
Arranges the association rules as a matrix with the itemsets in the antecedents
on one axis and the itemsets in the consequent on the other.  The interest
measure is either visualized by a color (darker means a higher value for the
measure) or as the height of a bar (method "matrix3D").

The list of control parameters for this method is
\describe{
\item{"main"}{plot title}
\item{"type"}{defines the way the data is rendered: "grid", "image" 
or "3D" (scatterplot3d)}
\item{"reorder"}{ if \code{TRUE} then the itemsets on the x and y-axes
are reordered to bring rules with similar values for the interest measure
closer together and make the plot clearer.}
\item{"orderBy"}{specifies the measure of
interest for reordering (default is the visualized measure)}
\item{"reorderMethod","reorderControl","reorderDist"}{seriation method, control arguments and distance method (default "euclidean")
used for reordering (see \code{seriate()} method in \pkg{seriation})}
\item{"col"}{ a vector of n colors used for the plot (default: 100 gray values)}
\item{"xlim","ylim"}{limits}
}

Currently there is no interactive version available.
}

\item{"grouped"}{
Grouped matrix-based visualization (Hahsler and Chelloboina, 2011). 
Antecedents (columns) in the matrix are
grouped using clustering. Groups are represented as balloons
in the matrix.

The list of control parameters for this method is
\describe{
\item{"main"}{plot title}
\item{"k"}{number of antecedent groups (default: 20)}
\item{"aggr.fun"}{aggregation function
can be any function computing a scalar from a vector
(e.g., min, mean, median (default), sum, max). It is also used
to reorder the balloons in the plot.}
\item{"col"}{color palette (default is 100 gray values.)}
}

Interactive manipulations are available. 
}

\item{"graph"}{
Represents the rules (or itemsets) as a graph.

Control arguments are 
\describe{
\item{"main"}{plot title}
\item{"cex"}{cex for labels}
\item{"itemLabels"}{display item/itemset names instead of ids (\code{TRUE})}
\item{"edgeCol", "nodeCol"}{color palettes for edges and nodes.}
\item{"measureLabels"}{display values of interest measures (\code{FALSE})}
\item{"precision"}{ number of digits for numbers in plot.}
\item{"type"}{vertices represent: "items" (default) or "itemsets"}
\item{"engine"}{graph layout engine: "igraph" (default) or "graphviz"}
\item{"layout"}{layout algorithm defined in \pkg{igraph} or \pkg{Rgraphviz}
    (default: igraph::nicely which usually does a Fruchterman-Reingold layout 
    for engine igraph and "dot"/"neato" for graphviz)}
\item{"arrowSize"}{[0,1]}
\item{"alpha"}{ alpha transparency value (default .8; set to 1 for no transparency)}
}

For the igraph engine the used plot function is \code{plot.igraph} 
in \pkg{igraph}. 
For graphviz the function \code{plot} in
\pkg{Rgraphviz} is used. Note that Rgraphviz is available at 
\url{http://www.bioconductor.org/}.
For the
interactive version \code{tkplot} in \pkg{igraph} is always used.

\code{...} arguments are passed on to the respective plotting function (use for color, etc.).
}

\item{"doubledecker", "mosaic"}{
Represents a single rule as a doubledecker or mosaic plot.
Parameter data has to be specified to compute the needed contingency
table.
Available control parameters are
\describe{
\item{"main"}{plot title}
}
}

\item{"paracoord"}{
Represents the rules (or itemsets) as a parallel coordinate plot.
Available control parameters are
\describe{
\item{"main"}{plot title}
\item{"reorder"}{reorder to minimize crossing lines.}
\item{"alpha"}{ alpha transparency value}
}

Currently there is no interactive version available.
}

\item{"iplots"}{
Experimental interactive plots (package \pkg{iplots}) 
which support selection, highlighting, 
brushing, etc. Currently plots a scatterplot (support vs. confidence) and
several histograms. Interactive manipulations are available. 
}

}
}
\value{
Several interactive plots return a set of selected rules/itemsets. Other plots
might return other data structures. For example graph-based
plots return the graph (invisibly).
}
\author{
Michael Hahsler and Sudheer Chelluboina. Some visualizations are based on 
the implementation by Martin Vodenicharov.
}
\references{
Bruzzese, D. and Davino, C. (2008), Visual Mining of Association Rules, in
Visual Data Mining: Theory, Techniques and Tools for Visual Analytics,
Springer-Verlag, pp. 103--122.

Hahsler M. and Chelluboina S. (2011), Visualizing association rules in
hierarchical groups. In 42nd Symposium on the Interface: Statistical, Machine
Learning, and Visualization Algorithms (Interface 2011). The Interface
Foundation of North America.
}

\seealso{
\code{\link[scatterplot3d]{scatterplot3d}} in \pkg{scatterplot3d},
\code{\link[igraph]{plot.igraph}} and
\code{\link[igraph]{tkplot}} in \pkg{igraph},
\code{\link[seriation]{seriate}} in \pkg{seriation}
}
\examples{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
rules

## Scatterplot
plot(rules)
## try: sel <- plot(rules, interactive=TRUE)

## Scatterplot with custom colors
library(colorspace) # for sequential_hcl
plot(rules, control=list(col=sequential_hcl(100)))
plot(rules, control=list(col=grey.colors(50, alpha =.8)))
    
## Two-key plot is a scatterplot with shading = "order"
plot(rules, shading="order", control=list(main = "Two-key plot", 
  col=rainbow(5)))

## The following techniques work better with fewer rules
subrules <- subset(rules, lift>2.5)
subrules
  
## 2D matrix with shading
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

## 3D matrix
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

## matrix with two measures
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), 
	control=list(reorder=TRUE))

## try: plot(subrules, method="matrix", measure="lift", interactive=TRUE, 
##		control=list(reorder=TRUE))

## grouped matrix plot
plot(rules, method="grouped")
## try: sel <- plot(rules, method="grouped", interactive=TRUE)

## graphs only work well with very few rules
subrules2 <- sample(rules, 10)
plot(subrules2, method="graph")
## igraph layout generators can be used (see ? igraph::layout_)
plot(subrules2, method="graph", control=list(layout=igraph::in_circle()))
plot(subrules2, method="graph", control=list(
  layout=igraph::with_graphopt(spring.const=5, mass=50)))

plot(subrules2, method="graph", control=list(type="itemsets"))
## try: plot(subrules2, method="graph", interactive=TRUE)
## try: plot(subrules2, method="graph", control=list(engine="graphviz"))


## parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

## Doubledecker plot only works for a single rule
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = Groceries)

## use iplots (experimental)
## try: sel <- plot(rules, method="iplots", interactive=TRUE)


## for itemsets
itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen=2))
plot(itemsets)
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", control=list(alpha=.5, reorder=TRUE))

## add more quality measures to use for the scatterplot
quality(itemsets) <- interestMeasure(itemsets, trans=Groceries)
head(quality(itemsets))
plot(itemsets, measure=c("support", "allConfidence"), shading="lift")
}
\keyword{ hplot }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
