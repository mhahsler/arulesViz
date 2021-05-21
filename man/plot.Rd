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
static standard plots (using base plots, \pkg{ggplot2}, \pkg{grid}), 
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
  \item{method}{ a string indicating the visualization method. Methods for rules  
    include "scatterplot", "two-key plot", "matrix", "grouped matrix",  "graph", "paracoord", etc. 
    Specify "help" to get a complete list of available methods. Note that some methods may 
    only be available for rules or itemsets.}
  \item{measure}{ measure(s) of interestingness 
    (e.g., "support", "confidence", "lift", "order") used in the visualization. Some 
    visualization methods need one measure, others take a vector with two 
    measures (e.g., scatterplot). In some plots (e.g., graphs) \code{NA} 
    can be used to suppress using a measure.}
  \item{shading}{ measure of interestingness used
    for the color of the points/arrows/nodes
    (e.g., "support", "confidence", "lift"). The default is "lift".
    \code{NA} can be often used to suppress shading.}
  \item{interactive}{ deprecated. See parameter \code{engine} below. }
  \item{engine}{ a string indicating the plotting engine used to 
    render the plot. 
    The "default" engine uses (mostly) \pkg{ggplot2}.
    Other engines include "base" (base R plots), 
    "grid", "interactive", "plotly", "visnetwork", "igraph", "graphviz", and "htmlwidget".
    Note that not all engines are available for all methods.
    Specify "help" to get 
    a complete list of available engines for the selected visualization method.
  }
  \item{data}{  the dataset (class "transactions") 
    used to generate the rules/itemsets. Only 
    "mosaic" and "doubledecker" require the original data.}
  \item{control}{ a list of control parameters for the plot. The available
    control parameters depend on the used visualization method and engine. Specify "help" to get 
    a complete list of available control parameters and their default values.}
  \item{\dots}{ Further arguments are added for convenience to the \code{control} list.}
}
\details{
Most visualization techniques are described by Bruzzese and Davino (2008),
however, we added more color shading, reordering and interactive features
(see Hahsler, 2017).
Many visualization methods take extra parameters as the \code{control} parameter list. Although, we have tried to keep control parameters consistent, the available control parameters vary from visualization method to visualization method. You can specift \code{"help"} for \code{method}, \code{engine}, or \code{control} to get a list of available settings.

Note on HTML widgets: HTML widgets tend to get very slow or unresponsive for too many rules. To prevent this situation, the control parameter \code{max} sets a limit, and the user is warned if the limit is reached.

The following visualization method are available:

\describe{
\item{"scatterplot", "two-key plot"}{ 
This visualization method draws a two dimensional scatterplot with different
measures of interestingness (parameter "measure") on the axes and a third 
measure (parameter "shading") is represented by the color of the points. 
There is a special value for shading called "order" which produces a
two-key plot where the color of the points represents the length (order) 
of the rule.
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
}
}

\item{"graph"}{
Represents the rules (or itemsets) as a graph with items as labeled vertices, and rules (or itemsets) represented as vertices connected to items using arrows. For rules, the LHS items are connected with arrows 
pointing to the vertex representing the rule and the RHS has an
arrow pointing to the item.
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
rules <- apriori(Groceries, parameter=list(support = 0.001, confidence = 0.8))
rules

## Getting help
# There are many method, plotting engines and all of them have different control parameters. Use
# "help" to get help. List available methods for the object rules:
plot(rules, method = "help")

# List the available engines for method "scatterplot"
plot(rules, method = "scatterplot", engine = "help")

# List control parameters for scatterplot with engine "ggplot2"
\dontrun{plot(rules, method = "scatterplot", engine = "ggplot2", control = "help")}


## Scatter plot
#  Display a scatter plot using two quality measures
plot(rules)

# Scatter plot with custom measures
plot(rules, measure = c("support", "lift"), shading = "confidence")

# Custom color scale, , labels, theme and no title (ggplot2)
library(ggplot2)
plot(rules, engine = "ggplot2", main = NULL) + 
  scale_color_gradient2(low = "red", mid = "gray90", high = "blue", 
    midpoint = 1, limits = c(0,12)) +
  labs(x = "Supp.", y = "Conf.", color = "Lift") + 
  theme_classic()

# Scatter plot using other engines
plot(rules, engine = "grid")
library(colorspace) # for sequential_hcl
plot(rules, engine = "grid", control = list(col=sequential_hcl(100)))

plot(rules, engine = "base")
  
# Interactive scatter plot using the grid engine (selected rules are returned)
\dontrun{sel <- plot(rules, engine = "interactive")}

# Create a html widget for interactive visualization (uses plotly)
\donttest{plot(rules, engine = "htmlwidget")}

# Two-key plot (a scatter plot with shading = "order")
plot(rules, method = "two-key plot")

  
## Matrix shading
#  Display rules as a matrix with RHS itemsets as rows and LHS itemsets as columns

# works better with small sets of rules
subrules <- subset(rules, lift > 5)
subrules

# 2D matrix with shading (ggplot2). The LHS and RHS are reordered so 
# that rules with similar lift are displayed close to each other.
plot(subrules, method = "matrix")

# Reorder rules differently
plot(subrules, method = "matrix", control = list(reorder = "none"))
plot(subrules, method = "matrix", control = list(reorder = "support/confidence"))
plot(subrules, method = "matrix", control = list(reorder = "similarity"))

# Use other engines
plot(subrules, method = "matrix", engine = "grid")
plot(subrules, method = "matrix", engine = "base")
plot(subrules, method = "matrix", engine = "3d")

# Interactive matrix plot 
# * Engine interactive: identify rules by clicking on them (click outside to end) 
# * Engine htmlwidget: hoover over rules to identify
\dontrun{plot(subrules, method = "matrix", engine = "interactive")}
\donttest{plot(subrules, method = "matrix", engine = "htmlwidget")}


## Grouped matrix plot
# Default engine is ggplot2
plot(rules, method="grouped matrix")
plot(rules, method="grouped matrix", shading = "confidence", k = 5)

# Create a htmlwidget
\donttest{plot(rules, method = "grouped matrix", engine = "htmlwidget")}

# Engine grid
plot(rules, method="grouped matrix", engine = "grid")
plot(rules, method="grouped matrix", engine = "grid", 
  col = grey.colors(10), 
  gp_labels = grid::gpar(col = "blue", cex=1, fontface="italic"))

# Interactive grouped matrix plot
\dontrun{sel <- plot(rules, method="grouped matrix", engine = "interactive")}

## Graph representation

# Graphs only work well with very few rules (we sample a few of the high-lift rules)
subrules2 <- sample(subset(rules, lift > 5), 25)

# Default engine is ggplot2 with ggnetwork
plot(subrules2, method = "graph")

# Specify layout (from igraph) and edge and node representation
# Note: The measures for size and color shading have to be specified in
# the control list in nodes!
plot(subrules2, method="graph", 
  control = list(
    layout = igraph::with_graphopt(),
    edges = ggnetwork::geom_edges(color = "grey80", 
      arrow = arrow(length = unit(5, "pt"), type = "open"), alpha = .5),
    nodes = ggnetwork::geom_nodes(aes(size = support, color = lift),  na.rm = TRUE),
    nodetext = ggnetwork::geom_nodetext(aes(label = label), alpha = .6)
  )) + 
  scale_color_gradient(low = "yellow", high = "red") + 
  scale_size(range = c(2, 15)) 

# Engine igraph
plot(subrules2, method = "graph", engine = "igraph")

# Custom colors
plot(subrules2, method = "graph", engine = "igraph",
  nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1)

# No shading for lift, set node color to gray and add 
# labels for support
plot(subrules2, method = "graph",  engine = "igraph", 
  shading = NA, nodeCol = grey(.5), measureLabels = TRUE)

# Use plot_options to alter any aspect of the graph 
# (see: https://igraph.org/r/doc/plot.common.html)
plot(subrules2, method = "graph", engine = "igraph",
  plot_options = list(
    edge.lty = 2, 
    vertex.label.cex = .6, 
    margin = c(.1,.1,.1,.1), 
    asp = .5))

# igraph layout generators can be used (see ? igraph::layout_)
plot(subrules2, method="graph", engine = "igraph", layout=igraph::in_circle())
plot(subrules2, method="graph", engine = "igraph",
  layout = igraph::with_graphopt(spring.const = 5, mass = 50))

# Graph rendering using engine graphviz
\dontrun{plot(subrules2, method = "graph", engine = "graphviz")}

# Default interactive plot (using igraph's tkplot)
\dontrun{plot(subrules2, method = "graph", engine = "interactive")}

# Interactive graph as a html widget (using igraph layout)
\donttest{
plot(subrules2, method = "graph", engine = "htmlwidget")
plot(subrules2, method = "graph", engine = "htmlwidget", 
  igraphLayout = "layout_in_circle")
}

## Parallel coordinates plot
plot(subrules2, method = "paracoord")

## Doubledecker and mosaic plot
# Uses functions in package vcd
# Notes: doubledecker and mosaic plots only visualize a single rule 
# and the transaction set is needed.
oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method = "doubledecker", data = Groceries)
plot(oneRule, method = "mosaic", data = Groceries)

## Visualizing itemsets
data(Groceries)
itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen = 2))

# default is a scatter plot with ggplot2
plot(itemsets)
plot(itemsets, engine = "grid")
plot(itemsets, engine = "base")

plot(itemsets, method = "graph")
plot(itemsets, method = "graph", engine = "igraph")

plot(itemsets, method = "paracoord", alpha = .5)

## Add more quality measures to use for the scatter plot

quality(itemsets) <- interestMeasure(itemsets, transactions = Groceries)
head(quality(itemsets))
plot(itemsets, measure = c("support", "allConfidence"), shading = "lift")

## Save HTML widget as web page
\dontrun{
p <- plot(rules, engine = "html")
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")}
# Note: self-contained seems to make the browser slow.
}
\keyword{ hplot }
