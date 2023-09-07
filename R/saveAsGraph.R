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


#' Convert rules or itemsets into a graph
#' 
#' Function to convert associations (rules, itemsets) into a igraph object and
#' saves the graph in different formats (e.g., GraphML, dimacs, dot).
#' 
#' Associations are represented as nodes: All items in the associations are
#' connected to the association node. For itemsets, the wdges are undirected,
#' for rules, the edges are directed towards the rhs
#' 
#' When associations are represented as edges: For rules, each item in the LHS
#' is connected with a directed edge to the item in the RHS. For itemsets,
#' undirected edges for each pair of item in the itemset are created.
#' 
#' @aliases saveAsGraph associations2igraph graph igraph tidygraph
#' @param x an object of class "rules" or "itemsets".
#' @param associationsAsNodes should associations be translated into nodes or
#' represented by edges?
#' @param file file name.
#' @param format file format (e.g., "edgelist", "graphml", "dimacs", "gml",
#' "dot"). See [`igraph::write.graph()`].
#' @param ...  further arguments are passed on to `associations2igraph()`.
#' @return \code{associations2igraph} returns an igraph object.
#' @author Michael Hahsler
#' @seealso \code{\link{plot}}, \code{\link[igraph]{write.graph}} in
#' \pkg{igraph}
#' @keywords file
#' @examples
#' 
#' data("Groceries")
#' rules <- apriori(Groceries, parameter=list(support = 0.01, confidence = 0.5))
#' 
#' # convert rules into a graph with rules as nodes
#' library("igraph")
#' g <- associations2igraph(rules)
#' g
#' 
#' plot(g)
#' 
#' # convert the graph into a tidygraph
#' library("tidygraph")
#' as_tbl_graph(g)
#' 
#' # convert the generating itemsets of the rules into a graph with itemsets as edges
#' itemsets <- generatingItemsets(rules)
#' itemsets 
#' g <- associations2igraph(itemsets, associationsAsNodes = FALSE)
#' g
#' 
#' plot(g, layout = layout_in_circle)
#' 
#' # save rules as a graph so they can be visualized using external tools
#' saveAsGraph(rules, "rules.graphml")
#' 
#' ## clean up
#' unlink("rules.graphml")
#'
#' @export
saveAsGraph <- function(x, file, format = "graphml", ...)
  igraph::write.graph(associations2igraph(x, ...), file, format = format)
