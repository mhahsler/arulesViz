\name{plot}
\Rdversion{1.1}
\alias{plot}
\alias{plot.itemsets}
\alias{plot.rules}
\alias{plot.grouped_matrix}
\alias{plotly}
\title{Visualize Association Rules and Itemsets}
\description{
Methods (S3) to visualize association rules and itemsets.
Implemented are several popular visualization methods 
including scatter plots with shading (two-key plots), 
graph based visualizations, doubledecker plots, etc.

Many plots can use different rendering engines including
static standard plots (mostly using \pkg{grid}), 
standard plots with interactive manipulation 
and interactive HTML widget-based visualizations.
}
\usage{
\method{plot}{rules}(x, method = NULL, measure = "support", shading = "lift", 
    interactive = NULL, engine = "default", data = NULL, control = NULL, ...)
\method{plot}{itemsets}(x, method = NULL, measure = "support", shading = NA,
    interactive = NULL, engine = "default", data = NULL, control = NULL, ...)
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets". }
  \item{method}{ a string with value "scatterplot", "two-key plot", "matrix", 
  "matrix3D",  "mosaic", 
  "doubledecker", "graph", "paracoord" or "grouped", "iplots" selecting the 
  visualization method (see Details). Note that some methods may only be  available for rules or itemsets.}
  
  \item{measure}{ measure(s) of interestingness 
  (e.g., "support", "confidence", "lift", "order") used in the visualization. Some 
  visualization methods need one measure, others take a vector with two 
  measures (e.g., scatterplot). In some plots (e.g., graphs) \code{NA} 
  can be used to suppress using a measure. }
  
  \item{shading}{ measure of interestingness used
  for the color of the points/arrows/nodes
  (e.g., "support", "confidence", "lift"). The default is "lift".
  \code{NA} can be often used to suppress shading.}
  
  \item{interactive}{ deprecated. See parameter \code{engine} below. }
  
  \item{control}{ a list of control parameters for the plot. The available
  control parameters depend on the used visualization method (see Details).}
  
  \item{data}{  the dataset (class "transactions") 
  used to generate the rules/itemsets. Only 
  "mosaic" and "doubledecker" require the original data.}
  
  \item{engine}{ a string indicating the plotting engine used to 
    render the plot. 
    The "default" engine uses (mostly) \pkg{grid}, 
    but some plots can produce interactive 
    interactive grid visualizations using engine "interactive", or
    HTML widgets using engine 
    "htmlwidget". These widgets can be saved as stand-alone web pages 
    (see Examples). Note that HTML widgets tend to get very slow 
    or unresponsive for 
    too many rules. To prevent this situation, the control parameter 
    \code{max} sets a limit, and the user is warned if the limit is reached. 
  }
  
  \item{\dots}{ Further arguments are added for convenience to the \code{control} list.}
}
\details{
Most visualization techniques are described by Bruzzese and Davino (2008),
however, we added more color shading, reordering and interactive features
(see Hahsler, 2017).
Many visualization methods take extra parameters as the \code{control} parameter list. Although, we have tried to keep control parameters consistent, the available control parameters vary (slightly) from visualization method to visualization method. A complete list of parameters with default
values can be obtained using verbose mode. For example, 

\code{plot(rules, method = "graph", control = list(verbose = TRUE))} 

prints a complete list of control parameters for method "graph" 
(default engine).

The following visualization method are available:

\describe{
\item{"scatterplot", "two-key plot"}{ 
This visualization method draws a two dimensional scatterplot with different
measures of interestingness (parameter "measure") on the axes and a third 
measure (parameter "shading") is represented by the color of the points. 
There is a special value for shading called "order" which produces a
two-key plot where the color of the points represents the length (order) 
of the rule.

Interactive manipulations are available. 
Engine "htmlwidget" is available to produce an interactive web-based
visualization (uses \pkg{plotly}).
}

\item{"matrix"}{ 
Arranges the association rules as a matrix with the itemsets in the antecedents
on one axis and the itemsets in the consequents on the other.  The
measure of interestingness (first element of \code{measure}) is either visualized by a color (darker means a higher value for the
measure) or as the height of a bar (engine "3d"). 
The control parameter \code{reorder} takes the values 
\code{"none"}, \code{"measure"}, \code{"support/confidence"}, or \code{"similarity"} and 
can be used to reorder LHS and RHS of the rules differntly. The default reordering average measure 
(typically lift) pushing the rules with the highest lift value to the top-left corner of the plot.   
Interactive
visualizations using engine "interactive" or "htmlwidget" (via \pkg{plotly})
are available.
}

\item{"grouped matrix"}{
Grouped matrix-based visualization (Hahsler and Karpienko, 2016; Hahsler 2016). 
Antecedents (columns) in the matrix are
grouped using clustering. Groups are represented by the most
interesting item (highest ratio of support in the group to support in all rules) 
in the group. Balloons
in the matrix are used to represent with what consequent the antecedents are 
connected.

Interactive manipulations (zooming into groups and identifying rules) are available. 

The list of control parameters for this method includes:
\describe{
\item{"main"}{plot title}
\item{"k"}{number of antecedent groups (default: 20)}
\item{"rhs_max"}{maximal number of RHSs to show. The rest are 
  suppressed. (default: 10)}
\item{"lhs_items"}{number of LHS items shown (default: 2)}
\item{"aggr.fun"}{aggregation function
can be any function computing a scalar from a vector
(e.g., min, mean (default), median, sum, max). It is also used
to reorder the balloons in the plot.}
\item{"col"}{color palette (default is 100 heat colors.)}
\item{"gp_labels", "gp_main", "gp_labs", "gp_lines"}{\code{gpar()} objects used to specify color, font and font size for
    different elements.}
}
}

\item{"graph"}{
Represents the rules (or itemsets) as a graph with items as labeled vertices, and rules (or itemsets) represented as vertices connected to items using arrows. For rules, the LHS items are connected with arrows 
pointing to the vertex representing the rule and the RHS has an
arrow pointing to the item.

Several engines are available. The default engine uses \pkg{igraph} (\code{plot.igraph} and \code{tkplot} for the interactive visualization). 
\code{...} arguments are passed on to the respective plotting function (use for color, etc.).

Alternatively, the engines "graphviz" (\pkg{Rgraphviz}) and "htmlwidget" (\pkg{visNetwork}) are available.
Note that Rgraphviz has to be installed separately from 
\url{http://www.bioconductor.org/}.
}

\item{"doubledecker", "mosaic"}{
Represents a single rule as a doubledecker or mosaic plot.
Parameter \code{data} has to be specified to compute the needed contingency
table. No interactive version is available.
}

\item{"paracoord"}{
Represents the rules (or itemsets) as a parallel coordinate plot.
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
might return other data structures. For example, graph-based
plots return the graph (invisibly). Engine "htmlwidget" always returns an object of class htmlwidget. 
}
\author{
Michael Hahsler and Sudheer Chelluboina. Some visualizations are based on 
the implementation by Martin Vodenicharov.
}
\references{
Hahsler M (2017). arulesViz: Interactive Visualization of Association
Rules with R. \emph{R Journal,} 9(2):163-175. ISSN 2073-4859. \doi{10.32614/RJ-2017-047}.
    
Bruzzese, D. and Davino, C. (2008), Visual Mining of Association Rules, in
Visual Data Mining: Theory, Techniques and Tools for Visual Analytics,
Springer-Verlag, pp. 103-122. \doi{10.1007/978-3-540-71080-6}

Hahsler, M. and Karpienko, R. (2016), Visualizing Association Rules in Hierarchical Groups. \emph{Journal of Business Economics,} 87(3):17-335. \doi{10.1007/s11573-016-0822-8}

Hahsler, M. (2016), Grouping association rules using lift. In C. Iyigun, R. Moghaddess, and A. Oztekin, editors, 11th INFORMS Workshop on Data Mining and Decision Analytics (DM-DA 2016).
}

\seealso{
\code{\link[scatterplot3d]{scatterplot3d}} in \pkg{scatterplot3d},
\code{\link[igraph]{plot.igraph}} and
\code{\link[igraph]{tkplot}} in \pkg{igraph},
\code{\link[seriation]{seriate}} in \pkg{seriation}
}
\examples{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

## Scatterplot
## -----------
plot(rules)

## Scatterplot with custom colors
library(colorspace) # for sequential_hcl
plot(rules, control = list(col=sequential_hcl(100)))
plot(rules, col=sequential_hcl(100))
plot(rules, col=grey.colors(50, alpha =.8))

## See all control options using verbose
plot(rules, verbose = TRUE)

## Interactive plot (selected rules are returned)
\dontrun{
sel <- plot(rules, engine = "interactive")}

## Create a html widget for interactive visualization
\dontrun{
plot(rules, engine = "htmlwidget")}

## Two-key plot (is a scatterplot with shading = "order")
plot(rules, method = "two-key plot")

  
## Matrix shading
## --------------

## The following techniques work better with fewer rules
subrules <- subset(rules, lift>5)
subrules

## 2D matrix with shading
plot(subrules, method="matrix")

## 3D matrix
plot(subrules, method="matrix", engine = "3d")

## Matrix with two measures
plot(subrules, method="matrix", shading=c("lift", "confidence"))

## Reorder rules
plot(subrules, method="matrix", control = list(reorder = "none"))
plot(subrules, method="matrix", control = list(reorder = "support/confidence"))
plot(subrules, method="matrix", control = list(reorder = "similarity"))


## Interactive matrix plot (default interactive and as a html widget)
\dontrun{
plot(subrules, method="matrix", engine="interactive")
plot(subrules, method="matrix", engine="htmlwidget")}

## Grouped matrix plot
## -------------------

plot(rules, method="grouped matrix")
plot(rules, method="grouped matrix", 
  col = grey.colors(10), 
  gp_labels = gpar(col = "blue", cex=1, fontface="italic"))

## Interactive grouped matrix plot
\dontrun{
sel <- plot(rules, method="grouped", engine = "interactive")}

## Graphs
## ------

## Graphs only work well with very few rules
subrules2 <- sample(subrules, 25)

plot(subrules2, method="graph")

## Custom colors
plot(subrules2, method="graph", 
  nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1)

## igraph layout generators can be used (see ? igraph::layout_)
plot(subrules2, method="graph", layout=igraph::in_circle())
plot(subrules2, method="graph", 
  layout=igraph::with_graphopt(spring.const=5, mass=50))

## Graph rendering using Graphviz
\dontrun{
plot(subrules2, method="graph", engine="graphviz")}

## Default interactive plot (using igraph's tkplot)
\dontrun{
plot(subrules2, method="graph", engine = "interactive")}

## Interactive graph as a html widget (using igraph layout)
\dontrun{
plot(subrules2, method="graph", engine="htmlwidget")
plot(subrules2, method="graph", engine="htmlwidget", 
  igraphLayout = "layout_in_circle")
}

## Parallel coordinates plot
## -------------------------

plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control = list(reorder=TRUE))

## Doubledecker plot 
## -----------------

## Note: only works for a single rule
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = Groceries)

## Itemsets
## --------

itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen=2))
plot(itemsets)
plot(itemsets, method="graph")
plot(itemsets, method="paracoord", alpha=.5, reorder=TRUE)

## Add more quality measures to use for the scatterplot
## ----------------------------------------------------

quality(itemsets) <- interestMeasure(itemsets, trans=Groceries)
head(quality(itemsets))
plot(itemsets, measure=c("support", "allConfidence"), shading="lift")

## Save HTML widget as web page
## ----------------------------
\dontrun{
p <- plot(rules, engine = "html")
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")}
# Note: selfcontained seems to make the browser slow.
}
\keyword{ hplot }
