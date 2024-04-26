#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyright (C) 2021 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Visualize Association Rules and Itemsets
#'
#' Methods (S3) to visualize association rules and itemsets. Implemented are
#' several popular visualization methods including scatter plots with shading
#' (two-key plots), graph based visualizations, doubledecker plots, etc.
#'
#' Many plots can use different rendering engines including static standard
#' plots (using base plots, \pkg{ggplot2}, \pkg{grid}), standard plots with
#' interactive manipulation and interactive HTML widget-based visualizations.
#'
#' Most visualization techniques are described by Bruzzese and Davino (2008),
#' however, we added more color shading, reordering and interactive features
#' (see Hahsler, 2017). Many visualization methods take extra parameters as the
#' `control` parameter list. Although, we have tried to keep control
#' parameters consistent, the available control parameters vary from
#' visualization method to visualization method. You can specift `"help"`
#' for `method`, `engine`, or `control` to get a list of
#' available settings.
#'
#' Note on HTML widgets: HTML widgets tend to get very slow or unresponsive for
#' too many rules. To prevent this situation, the control parameter `max`
#' sets a limit, and the user is warned if the limit is reached.
#'
#' The following visualization method are available:
#'
#' \describe{
#' \item{"scatterplot", "two-key plot"}{ This visualization method
#' draws a two dimensional scatterplot with different measures of
#' interestingness (parameter "measure") on the axes and a third measure
#' (parameter "shading") is represented by the color of the points.  There is a
#' special value for shading called "order" which produces a two-key plot where
#' the color of the points represents the length (order) of the rule. }
#'
#' \item{"matrix"}{ Arranges the association rules as a matrix with the
#' itemsets in the antecedents on one axis and the itemsets in the consequents
#' on the other.  The measure of interestingness (first element of
#' `measure`) is either visualized by a color (darker means a higher value
#' for the measure) or as the height of a bar (engine "3d").  The control
#' parameter `reorder` takes the values `"none"`, `"measure"`,
#' `"support/confidence"`, or `"similarity"` and can be used to
#' reorder LHS and RHS of the rules differntly. The default reordering average
#' measure (typically lift) pushing the rules with the highest lift value to
#' the top-left corner of the plot.  }
#'
#' \item{"grouped matrix"}{ Grouped matrix-based visualization (Hahsler and
#' Karpienko, 2016; Hahsler 2016).  Antecedents (columns) in the matrix are
#' grouped using clustering. Groups are represented by the most interesting
#' item (highest ratio of support in the group to support in all rules) in the
#' group. Balloons in the matrix are used to represent with what consequent the
#' antecedents are connected.
#'
#' Interactive manipulations (zooming into groups and identifying rules) are
#' available.
#'
#' The list of control parameters for this method includes:
#' \describe{
#'
#' \item{"main"}{plot title}
#' \item{"k"}{number of antecedent groups (default:
#' 20)}
#' \item{"rhs_max"}{maximal number of RHSs to show. The rest are
#' suppressed. (default: 10)}
#' \item{"lhs_items"}{number of LHS items shown
#' (default: 2)}
#' \item{"aggr.fun"}{aggregation function can be any function
#' computing a scalar from a vector (e.g., min, mean (default), median, sum,
#' max). It is also used to reorder the balloons in the plot.}
#' \item{"col"}{color palette (default is 100 heat colors.)} } }
#'
#' \item{"graph"}{ Represents the rules (or itemsets) as a graph with items as
#' labeled vertices, and rules (or itemsets) represented as vertices connected
#' to items using arrows. For rules, the LHS items are connected with arrows
#' pointing to the vertex representing the rule and the RHS has an arrow
#' pointing to the item. }
#'
#' \item{"doubledecker", "mosaic"}{ Represents a single rule as a doubledecker
#' or mosaic plot. Parameter `data` has to be specified to compute the
#' needed contingency table. No interactive version is available. }
#'
#' \item{"paracoord"}{ Represents the rules (or itemsets) as a parallel
#' coordinate plot. Currently there is no interactive version available. }
#'
#' }
#'
#' @name plot_arulesViz
#'
#' @aliases plot plot.itemsets plot.rules plot.grouped_matrix plotly
#' guide_edge_colourbar
#' @param x an object of class "rules" or "itemsets".
#' @param method a string indicating the visualization method. Methods for
#' rules include "scatterplot", "two-key plot", "matrix", "grouped matrix",
#' "graph", "paracoord", etc.  Specify "help" to get a complete list of
#' available methods. Note that some methods may only be available for rules or
#' itemsets.
#' @param measure measure(s) of interestingness (e.g., "support", "confidence",
#' "lift", "order") used in the visualization. Some visualization methods need
#' one measure, others take a vector with two measures (e.g., scatterplot). In
#' some plots (e.g., graphs) `NA` can be used to suppress using a measure.
#' @param shading measure of interestingness used for the color of the
#' points/arrows/nodes (e.g., "support", "confidence", "lift"). The default is
#' "lift".  `NULL` can be often used to suppress shading.
#' @param limit A limit on the number of associations displayed. The top limit
#' associations according to the measure specified in shading are chosen.
#' @param interactive deprecated. See parameter `engine` below.
#' @param engine a string indicating the plotting engine used to render the
#' plot.  The "default" engine uses (mostly) \pkg{ggplot2}.  Other engines
#' include "base" (base R plots), "grid", "interactive", "plotly",
#' "visnetwork", "igraph", "graphviz", and "htmlwidget" (which can be embedded
#' in RMarkdown).  Note that not all engines are available for all methods.
#' Specify "help" to get a complete list of available engines for the selected
#' visualization method.
#' @param data the dataset (class "transactions") used to generate the
#' rules/itemsets. Only "mosaic" and "doubledecker" require the original data.
#' @param control a list of control parameters for the plot. The available
#' control parameters depend on the used visualization method and engine.
#' Specify "help" to get a complete list of available control parameters and
#' their default values.
#' @param \dots Further arguments are added for convenience to the
#' `control` list.
#' @return Several interactive plots return a set of selected rules/itemsets.
#' Other plots might return other data structures. For example, graph-based
#' plots return the graph (invisibly). Engine "htmlwidget" always returns an
#' object of class htmlwidget.
#' @author Michael Hahsler and Sudheer Chelluboina. Some visualizations are
#' based on the implementation by Martin Vodenicharov.
#' @seealso [scatterplot3d::scatterplot3d()],
#' [igraph::plot.igraph()] and [igraph::tkplot()], 
#' [seriation::seriate()].
#' @references Hahsler M (2017). arulesViz: Interactive Visualization of
#' Association Rules with R. _R Journal,_ 9(2):163-175. ISSN 2073-4859.
#' \doi{10.32614/RJ-2017-047}
#'
#' Bruzzese, D. and Davino, C. (2008), Visual Mining of Association Rules, in
#' Visual Data Mining: Theory, Techniques and Tools for Visual Analytics,
#' Springer-Verlag, pp. 103-122.
#' \doi{10.1007/978-3-540-71080-6}
#'
#' Hahsler, M. and Karpienko, R. (2016), Visualizing Association Rules in
#' Hierarchical Groups. _Journal of Business Economics,_ 87(3):17-335.
#' \doi{10.1007/s11573-016-0822-8}
#'
#' Hahsler, M. (2016), Grouping association rules using lift. In C. Iyigun, R.
#' Moghaddess, and A. Oztekin, editors, 11th INFORMS Workshop on Data Mining
#' and Decision Analytics (DM-DA 2016).
#' @keywords hplot
#' @examples
#'
#' # Note: To speed example execution, not all examples are not run when using example("plot").
#' # Use example("plot") to run all examples.
#'
#' data(Groceries)
#' rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.8))
#' rules
#'
#' ## Getting help
#' # There are many method, plotting engines and all of them have different control parameters. Use
#' # "help" to get help. List available methods for the object rules:
#' plot(rules, method = "help")
#'
#' # List the available engines for method "scatterplot"
#' plot(rules, method = "scatterplot", engine = "help")
#'
#' \dontrun{
#' # List control parameters for scatterplot with engine "ggplot2"
#' plot(rules, method = "scatterplot", engine = "ggplot2", control = "help")
#' }
#'
#'
#' ## Scatter plot
#' #  Display a scatter plot using two quality measures
#' plot(rules)
#'
#' # Scatter plot with custom measures and limiting the plot to the 100 with the
#' # largest value for for the shading measure.
#' plot(rules, measure = c("support", "lift"), shading = "confidence", limit = 100)
#'
#' \dontrun{
#' # Custom color scale, labels, theme and no title (ggplot2)
#' library(ggplot2)
#' plot(rules, engine = "ggplot2", main = NULL, limit = 100) +
#'   scale_color_gradient2(
#'     low = "red", mid = "gray90", high = "blue",
#'     midpoint = 1, limits = c(0, 12)
#'   ) +
#'   labs(x = "Supp.", y = "Conf.", color = "Lift") +
#'   theme_classic()
#'
#' # Interactive scatter plot using the grid engine (selected rules are returned)
#' if (interactive()) {
#'   sel <- plot(rules, engine = "interactive")
#'
#'   # Create a html widget for interactive visualization (uses plotly)
#'   plot(rules, engine = "htmlwidget")
#' }
#' }
#'
#' # Two-key plot (a scatter plot with shading = "order")
#' plot(rules, method = "two-key plot", limit = 100)
#'
#'
#' ## Matrix shading
#' #  Display rules as a matrix with RHS itemsets as rows and LHS itemsets as columns
#'
#' # works better with small sets of rules
#' subrules <- subset(rules, lift > 5)
#' subrules
#'
#' # 2D matrix with shading (ggplot2). The LHS and RHS are reordered so
#' # that rules with similar lift are displayed close to each other.
#' plot(subrules, method = "matrix")
#'
#' \dontrun{
#' # Interactive matrix plot
#' # * Engine interactive: identify rules by clicking on them (click outside to end)
#' # * Engine htmlwidget: hoover over rules to identify
#' if (interactive()) {
#'   plot(subrules, method = "matrix", engine = "interactive")
#'   plot(subrules, method = "matrix", engine = "htmlwidget")
#' }
#' }
#'
#'
#' ## Grouped matrix plot
#' # Default engine is ggplot2
#' plot(rules, method = "grouped matrix", k = 5)
#'
#' \dontrun{
#' # Create a htmlwidget
#' plot(rules, method = "grouped matrix", engine = "htmlwidget")
#'
#' # Interactive grouped matrix plot
#' if (interactive()) {
#'   sel <- plot(rules, method = "grouped matrix", engine = "interactive")
#' }
#' }
#'
#' ## Graph representation
#' # Default engine is ggplot2 with ggraph. Associations are represented as nodes.
#' # We limit the number of rules to the 10 with the larges
#' # lift (measure used for shading)
#' plot(subrules, method = "graph", limit = 10)
#'
#' \dontrun{
#' # Circular layout (see? ggraph for the meaning of the arguments)
#' plot(subrules, method = "graph", layout = "linear", circular = TRUE, limit = 10)
#'
#' # Use igraph layouts (algorithm is passes on as ... to ggraph)
#' plot(subrules,
#'   method = "graph", layout = "igraph",
#'   ggraphdots = list(algorithm = "graphopt", spring.const = 1, mass = 10), limit = 10
#' )
#'
#' # Specify edge and node representation
#' library(ggplot2)
#' plot(subrules,
#'   method = "graph",
#'   control = list(
#'     edges = ggraph::geom_edge_link(
#'       end_cap = ggraph::circle(4, "mm"),
#'       start_cap = ggraph::circle(4, "mm"),
#'       color = "black",
#'       arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
#'       alpha = .2
#'     ),
#'     nodes = ggraph::geom_node_point(aes(size = support, color = lift)),
#'     nodetext = ggraph::geom_node_label(aes(label = label), alpha = .8, repel = TRUE)
#'   ),
#'   limit = 10
#' ) +
#'   scale_color_gradient(low = "yellow", high = "red") +
#'   scale_size(range = c(2, 10))
#'
#' # ggplot also can represent associations as edges. Here a rules is represented as a set of
#' # arrows going from the LHS items to the RHS item.
#' plot(subrules, method = "graph", asEdges = TRUE, limit = 10)
#' plot(subrules, method = "graph", asEdges = TRUE, circular = FALSE, limit = 10)
#' }
#'
#' # Engine igraph
#' plot(subrules, method = "graph", engine = "igraph", limit = 10)
#' plot(subrules,
#'   method = "graph", engine = "igraph",
#'   nodeCol = grey.colors(10), edgeCol = grey(.7), alpha = 1,
#'   limit = 10
#' )
#'
#' # Use plot_options to alter any aspect of the graph
#' # (see: https://igraph.org/r/doc/plot.common.html)
#' plot(subrules,
#'   method = "graph", engine = "igraph",
#'   plot_options = list(
#'     edge.lty = 2,
#'     vertex.label.cex = .6,
#'     margin = c(.1, .1, .1, .1),
#'     asp = .5
#'   ),
#'   limit = 10
#' )
#'
#' # igraph layout generators can be used (see ? igraph::layout_)
#' plot(subrules, method = "graph", engine = "igraph", layout = igraph::in_circle(), limit = 10)
#'
#' \dontrun{
#' # Graph rendering using engine graphviz
#' plot(subrules, method = "graph", engine = "graphviz", limit = 10)
#'
#' if (interactive()) {
#'   # Default interactive plot (using igraph's tkplot)
#'   plot(subrules, method = "graph", engine = "interactive", limit = 10)
#'
#'   # Interactive graph as a html widget (using igraph layout)
#'   plot(subrules, method = "graph", engine = "htmlwidget", limit = 10)
#'   plot(subrules,
#'     method = "graph", engine = "htmlwidget",
#'     igraphLayout = "layout_in_circle", limit = 10
#'   )
#' }
#' }
#'
#' ## Parallel coordinates plot
#' plot(subrules, method = "paracoord", limit = 10)
#'
#'
#' ## Doubledecker and mosaic plot
#' # Uses functions in package vcd
#' # Notes: doubledecker and mosaic plots only visualize a single rule
#' # and the transaction set is needed.
#' oneRule <- sample(rules, 1)
#' inspect(oneRule)
#' plot(oneRule, method = "doubledecker", data = Groceries)
#'
#'
#' ## Visualizing itemsets
#' data(Groceries)
#' itemsets <- eclat(Groceries, parameter = list(support = 0.02, minlen = 2))
#'
#' # default is a scatter plot with ggplot2
#' plot(itemsets)
#'
#' plot(itemsets, method = "graph", limit = 10)
#'
#' \dontrun{
#' plot(itemsets, method = "graph", asEdges = TRUE, limit = 10)
#' plot(itemsets, method = "graph", asEdges = TRUE, circular = FALSE, limit = 10) +
#'   theme(plot.margin = margin(10, 10, 30, 20, "mm"))
#' }
#'
#' plot(itemsets, method = "paracoord", alpha = .5, limit = 10)
#'
#' # Add more quality measures to use for the scatter plot
#' quality(itemsets) <- interestMeasure(itemsets, transactions = Groceries)
#' head(quality(itemsets))
#' plot(itemsets, measure = c("support", "allConfidence"), shading = "lift")
#'
#' \dontrun{
#' # Save HTML widget as web page
#' p <- plot(rules, engine = "html")
#' htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
#' # Note: self-contained seems to make the browser slow.
#'
#' # inspect the widget
#' browseURL("arules.html")
#'
#' # clean up
#' unlink(c("arules.html", "arules_files"), recursive = TRUE)
#' }
#'
#' @export
plot.rules <- function(
    x,
    method = NULL,
    measure = "support",
    shading = "lift",
    limit = NULL,
    interactive = NULL,
    engine = "default",
    data = NULL,
    control = NULL,
    ...) {
  methods <- c(
    "matrix",
    "mosaic",
    "doubledecker",
    "graph",
    "paracoord",
    "scatterplot",
    "grouped matrix",
    "two-key plot",
    "matrix3D"
  )

  if (!is.null(method) && method == "help") {
    message(
      "Available methods for plotting rules are:\n",
      paste0(methods, collapse = ", ")
    )
    return(invisible(methods))
  }

  if (length(x) < 1) {
    stop("x contains 0 rules!")
  }

  ## add order and id
  quality(x)$order <- size(x)
  quality(x)$id <- seq_along(x)

  ## default is a scatter plot
  if (is.null(method)) {
    methodNr <- 6
  } else {
    methodNr <- pmatch(tolower(method), tolower(methods))
  }
  if (is.na(methodNr)) {
    stop(paste(
      "Unknown method:",
      sQuote(method),
      "\nAvailable methods:",
      paste(sQuote(methods), collapse = ", ")
    ))
  }

  ## complete measure and shading
  mid <- pmatch(measure, colnames(quality(x)), duplicates.ok = TRUE)
  if (any(is.na(mid))) {
    stop(
      "Measure not available in rule set: ",
      paste(sQuote(measure[is.na(mid)]), collapse = ", ")
    )
  }
  measure <- colnames(quality(x))[mid]

  ## NULL means no shading
  if (!is.null(shading)) {
    sid <- pmatch(shading, colnames(quality(x)))
    if (any(is.na(sid))) {
      stop(
        "Shading measure not available in rule set: ",
        paste(sQuote(shading[is.na(sid)]), collapse = ", ")
      )
    }
    shading <- colnames(quality(x))[sid]
  }

  ## limit
  x <- limit(x, limit, shading, measure, quiet = TRUE)

  ## add interactive and engine
  if (!is.null(interactive)) {
    warning("The parameter interactive is deprecated. Use engine='interactive' instead.")
    if (engine == "default" && interactive) {
      engine <- "interactive"
    }
  } else {
    interactive <- FALSE
  }

  ### if control is character then I think it is "help"
  if (is.character(control)) {
    control <- list(help = TRUE)
  }
  if (is.null(control$engine)) {
    control$engine <- engine
  }

  ## work horses
  if (methodNr == 1) {
    matrixplot(x, measure = shading, control, ...)
  } else if (methodNr == 2) {
    doubledeckerplot(x,
      measure = measure,
      data = data,
      c(control, list(type = "mosaic")),
      ...
    )
  } else if (methodNr == 3) {
    doubledeckerplot(x,
      measure = measure,
      data = data,
      c(control, list(type = "doubledecker")),
      ...
    )
  } else if (methodNr == 4) {
    graphplot(x,
      measure = measure,
      shading = shading, control, ...
    )
  } else if (methodNr == 5) {
    paracoord_rules(x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    )
  } else if (methodNr == 6) {
    if (length(measure) < 2) {
      measure[2] <- "confidence"
    }
    scatterplot(x, measure = measure, shading = shading, control, ...)
  } else if (methodNr == 7) {
    grouped_matrix_plot(x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    )
  } else if (methodNr == 8) {
    if (is.null(control$col)) {
      control$col <- grDevices::rainbow(max(size(x)) - 1L)
    }
    scatterplot(
      x,
      measure = c("support", "confidence"),
      shading = "order",
      control,
      ...
    )
  } else if (methodNr == 9) {
    warning("method 'matrix3D' is deprecated use method 'matrix' with engine '3d'")
    control$engine <- "3d"
    matrixplot(x, measure = shading, control = control, ...)
  }
}

#' @rdname plot_arulesViz
#' @export
plot.itemsets <- function(
    x,
    method = NULL,
    measure = "support",
    shading = NULL,
    limit = NULL,
    interactive = NULL,
    engine = "default",
    data = NULL,
    control = NULL,
    ...) {
  ## methods
  methods <- c(
    "graph",
    "paracoord",
    "scatterplot"
  )

  if (!is.null(method) && method == "help") {
    message(
      "Available methods for plotting itemsets are:\n",
      paste0(methods, collapse = ", ")
    )
    return(invisible(methods))
  }

  ## add interactive and engine
  if (is.null(control$engine)) {
    control$engine <- engine
  }
  if (!is.null(interactive)) {
    warning("The parameter interactive is deprecated. Use engine='interactive' instead.")
    if (engine == "default" && interactive) {
      engine <- "interactive"
    }
  } else {
    interactive <- FALSE
  }

  if (length(x) < 1) {
    stop("x contains 0 itemsets!")
  }

  ## add order
  quality(x)$order <- size(x)
  quality(x)$id <- seq_along(x)

  ## limit
  x <- limit(x, limit, shading, measure, quiet = TRUE)

  if (is.null(method)) {
    methodNr <- 3
  } else {
    methodNr <- pmatch(tolower(method), tolower(methods))
  }
  if (is.na(methodNr)) {
    stop(paste(
      "Unknown method:",
      sQuote(method),
      "\nAvailable methods:",
      paste(sQuote(methods), collapse = ", ")
    ))
  }


  ## work horses
  if (methodNr == 1) {
    graphplot(x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    )
  } else if (methodNr == 2) {
    paracoord_items(x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    )
  } else if (methodNr == 3) {
    if (length(measure) < 2) {
      measure[2] <- "order"
    }
    scatterplot(x, measure = measure, shading = shading, control, ...)
  }
}
