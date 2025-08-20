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

graph_ggplot2 <- function(
    x,
    measure = "support",
    shading = "lift",
    control = NULL, asEdges = NULL,
    ...) {
  if (!is.null(asEdges) && asEdges) {
    graph_edges_ggplot2(x, measure, shading, control, ...)
  } else {
    graph_nodes_ggplot2(x, measure, shading, control, ...)
  }
}

graph_nodes_ggplot2 <- function(
    x,
    measure = "support",
    shading = "lift",
    control = NULL,
    ...) {
  if (!is.null(shading) && (all(is.na(shading)) || is.null(quality(x)[[shading]]))) {
    shading <- NULL
  }

  # NULLify for checker
  label <- NULL
  type <- NULL

  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      # main = paste("Graph for", length(x), "rules"),
      layout = "stress",
      circular = FALSE,
      ggraphdots = NULL,
      edges = ggraph::geom_edge_link(
        ### FIXME: dynamic length!
        # aes_string(
        #   end_cap = paste0("circle(node2.", measure, ", unit = 'native')"),
        #   start_cap = paste0("circle(node1.", measure, ", unit = 'native')"),
        #   ),
        # 3 mm is the radius for size 6 points
        end_cap = ggraph::circle(3, "mm"),
        start_cap = ggraph::circle(3, "mm"),
        color = "gray80",
        arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"),
        alpha = .2
      ),
      nodes = if (!is.null(shading)) {
        ggraph::geom_node_point(aes(size = .data[[measure]], color = .data[[shading]], filter = type == 2))
      } else {
        ggraph::geom_node_point(aes(size = .data[[measure]], filter = type == 2),
          color = default_colors(1), alpha = .3
        )
      },
      nodetext = ggraph::geom_node_text(aes(label = label, filter = type == 1), repel = TRUE),
      colors = default_colors(2),
      engine = "ggplot2",
      max = 100
    )
  )

  x <- limit(x, control$max, shading, measure)
  g <- associations2igraph(x)

  # complains about missing values for points (na.rm = TRUE does not currently work)
  do.call(
    ggraph::ggraph,
    c(list(graph = g, layout = control$layout, circular = control$circular), control$ggraphdots)
  ) +
    # ggraph::ggraph(g, layout = control$layout, circular = control$circular) +
    control$edges +
    control$nodes +
    control$nodetext +
    scale_color_gradient(
      low = control$colors[2],
      high = control$colors[1],
      na.value = "#FFFFFFFF"
    ) +
    # base_family is a problem for latex
    ggraph::theme_graph(base_family = "") +
    coord_cartesian(clip = "off")
}

graph_edges_ggplot2 <- function(
    x,
    measure = "support",
    shading = "lift",
    control = NULL,
    ...) {
  if (!is.null(shading) && (all(is.na(shading)) || is.null(quality(x)[[shading]]))) {
    shading <- NULL
  }

  # NULLify
  label <- NULL

  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      # main = paste("Graph for", length(x), class(x)),
      layout = "linear",
      circular = TRUE,
      ggraphdots = NULL,
      edges = ggraph::geom_edge_arc(
        ### FIXME: dynamic length!
        # aes_string(
        #   end_cap = paste0("circle(node2.", measure, ", unit = 'native')"),
        #   start_cap = paste0("circle(node1.", measure, ", unit = 'native')"),
        #   ),
        # 3 mm is the radius for size 6 points
        if(!is.null(shading)) { 
          aes(edge_colour = .data[[shading]], edge_width = .data[[measure]])
        } else {
          aes(edge_width = .data[[measure]])
        },
        end_cap = ggraph::circle(3, "mm"),
        start_cap = ggraph::circle(3, "mm"),
        arrow = if (is(x, "rules")) {
          arrow(length = unit(2, "mm"), angle = 20, type = "closed")
        } else {
          NULL
        },
        alpha = .7
      ),
      nodes = NULL,
      nodetext = if (!is.null(control$circular) && !control$circular) {
        ggraph::geom_node_text(aes(label = label), alpha = .8, angle = 45, hjust = 1)
      } else {
        ggraph::geom_node_text(aes(label = label), alpha = .8, repel = TRUE)
      },
      colors = default_colors(2),
      engine = "ggplot2",
      max = 100
    )
  )

  x <- limit(x, control$max, shading, measure)
  # FIXME: reduce overplotting when shading is used is not easy in igraph
  g <- associations2igraph(x, associationsAsNodes = FALSE)

  # complains about missing values for points (na.rm = TRUE does not currently work)
  do.call(
    ggraph::ggraph,
    c(list(graph = g, layout = control$layout, circular = control$circular), control$ggraphdots)
  ) +
    # ggraph::ggraph(g, layout = control$layout, circular = control$circular) +
    control$edges +
    control$nodes +
    control$nodetext +
    ggraph::scale_edge_width(range = c(0.5, 2)) +
    ggraph::scale_edge_color_gradient(
      low = control$colors[2],
      high = control$colors[1],
      na.value = "#FFFFFFFF",
    ) +
    # base_family is a problem for latex
    ggraph::theme_graph(base_family = "", plot_margin = margin(5, 5, 5, 5, unit = "mm")) +
    # no cliping for labels
    coord_cartesian(clip = "off")
}
