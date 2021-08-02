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


### default with alpha
.nodeColors <- function(alpha = NULL) {
  if (is.null(alpha))
    alpha <- 1
  c(grDevices::rgb(.4, .8, .4, alpha),
    grDevices::rgb(.6, .6, .8, alpha))
}

associations2igraph <- function(x, associationsAsNodes = TRUE) {
  if (associationsAsNodes) associations2igraph_nodes(x)
  else associations2igraph_edges(x)
}

associations2igraph_nodes <- function(x) {
  # only used items
  itemNodes <- which(itemFrequency(items(x),
    type = "absolute") > 0)
  assocNodes <- paste("assoc", 1:length(x), sep = '')
  
  ## add rhs for rules
  if (is(x, "rules")) {
    lhs <- LIST(lhs(x), decode = FALSE)
    from_lhs <- unlist(lhs)
    to_lhs <- assocNodes[rep(1:length(x), sapply(lhs, length))]
    rhs <- LIST(rhs(x), decode = FALSE)
    to_rhs <- unlist(rhs)
    from_rhs <- assocNodes[rep(1:length(x), sapply(rhs, length))]
  } else {
    ### not used for itemsets
    lhs <- LIST(items(x), decode = FALSE)
    from_lhs <- unlist(lhs)
    to_lhs <- assocNodes[rep(1:length(x), sapply(lhs, length))]
    from_rhs <- integer(0)
    to_rhs <- integer(0)
  }
  
  e.list <- cbind(c(from_lhs, from_rhs), c(to_lhs, to_rhs))
  v.labels <- data.frame(
    name = c(as.character(itemNodes), assocNodes),
    #label = c(itemLabels(x)[itemNodes], rep("", length(assocNodes))),
    label = c(itemLabels(x)[itemNodes], rep(NA, length(assocNodes))),
    index = c(itemNodes, seq_along(assocNodes)),
    type = c(rep(1, length(itemNodes)), rep(2, length(assocNodes))),
    stringsAsFactors = FALSE
  )
  
  g <-
    igraph::graph.data.frame(e.list, directed = is(x, "rules"), vertices = v.labels)
  
  ## add quality measures
  for (m in names(quality(x))) {
    g <- igraph::set.vertex.attribute(g, m, which(v.labels$type == 2),
      quality(x)[[m]])
  }
  g
}

associations2igraph_edges <- function(x) {
  # only used items
  itemNodes <- which(itemFrequency(items(x),
    type = "absolute") > 0)
  
  ## add rhs for rules
  if (is(x, "rules")) {
    lhs <- LIST(lhs(x), decode = FALSE)
    edges_per_assoc <- sapply(lhs, length) 
    lhs <- unlist(lhs)
    rhs <- unlist(LIST(rhs(x), decode = FALSE))
    rhs <- rep(rhs, times = edges_per_assoc)
    edges <- cbind(lhs, rhs) 
  } else {
    items <- LIST(items(x), decode = FALSE)
    edges <- lapply(items, utils::combn, 2)
    edges_per_assoc <- sapply(edges, ncol) 
    edges <- t(matrix(unlist(edges), nrow = 2))
  }
  
  e.list <- cbind(edges, index = rep(seq_along(x), times = edges_per_assoc))
  v.labels <- data.frame(
    name = as.character(itemNodes),
    label = itemLabels(x)[itemNodes],
    index = itemNodes,
    stringsAsFactors = FALSE
  )
  
  g <-
    igraph::graph.data.frame(e.list, directed = is(x, "rules"), vertices = v.labels)
  
  ## add quality measures
  for (m in names(quality(x))) {
    g <- igraph::set.edge.attribute(g, m,
      value = rep(quality(x)[[m]], times = edges_per_assoc))
  }
  g
}

graphplot <- function(x,
  measure = "support",
  shading = "lift",
  control = NULL,
  ...) {
  engines <-
    c(
      "default",
      "ggplot2",
      "igraph",
      "interactive",
      "graphviz",
      "visNetwork",
      "htmlwidget"
    )
  if (control$engine == "help") {
    message("Available engines for this plotting method are:\n",
      paste0(engines, collapse = ", "))
    return(invisible(engines))
  }
  
  ## check if shading measure is available
  if (!is.null(shading) && is.null(quality(x)[[shading]]))
    shading <- NULL
  
  m <- pmatch(control$engine, engines, nomatch = 0)
  if (m == 0)
    stop(
      "Unknown engine: ",
      sQuote(control$engine),
      " Valid engines: ",
      paste(sQuote(engines), collapse = ", ")
    )
  control$engine <- engines[m]
  
  ### FIXME: fix max and control
  if (pmatch(control$engine, c("visNetwork", "htmlwidget"), nomatch = 0) >
      0) {
    return(graph_visNetwork(
      x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    ))
    
  } else if (pmatch(control$engine,
    c("igraph", "interactive", "graphviz"),
    nomatch = 0) > 0) {
    return(graph_igraph(
      x,
      measure = measure,
      shading = shading,
      control = control,
      ...
    ))
  }
  
  ## default is ggplot2 with ggnetwork
  return(graph_ggplot2(
    x,
    measure = measure,
    shading = shading,
    control = control,
    ...
  ))
}