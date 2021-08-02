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


# FIXME: Implement max
# FIXME: size and color...

graph_visNetwork <-
  function(x,
    measure = "support",
    shading = "lift",
    control = NULL,
    ...) {
    
    control <- c(control, list(...))
    control <- .get_parameters(
      control,
      list(
        #main = paste("Graph for", length(x), "rules"),
        itemCol = grDevices::hcl(h = 260),
        nodeCol = default_colors(100, alpha = 0),
        precision = 3,
        igraphLayout = "layout_nicely",
        interactive = TRUE,
        engine = "visNetwork",
        max = 100,
        selection_menu = TRUE,
        degree_highlight = 1
      )
    )
    
    x <- limit(x, control$max, shading, measure)
    g <- associations2igraph(x)
    
    va <- igraph::get.vertex.attribute(g)
    n_items <- sum(va$type == 1)
    
    titleAssoc <- 
      if (is(x, "rules")) 
        paste0(
          '<B>[',
          1:length(x),
          ']</B><BR>',
          labels(
            x,
            itemSep = ',<BR>&nbsp;&nbsp;',
            ruleSep = '<BR>&nbsp;&nbsp; => ',
            setStart = '<B>{',
            setEnd = '}</B>'
          ),
          "<BR><BR>",
          apply(
            quality(x),
            MARGIN = 1,
            FUN = function(x)
              paste(names(x), "=", signif(x, control$precision), collapse = "<BR>")
          )
        )
    else
        paste0(
          '<B>[',
          1:length(x),
          ']</B><BR>',
          labels(
            x,
            itemSep = ',<BR>&nbsp;&nbsp;',
            setStart = '<B>{',
            setEnd = '}</B>'
          ),
          "<BR><BR>",
          apply(
            quality(x),
            MARGIN = 1,
            FUN = function(x)
              paste(names(x), "=", signif(x, control$precision), collapse = "<BR>")
          )
        )
           
    title <- c(va$label[va$type == 1], titleAssoc)
    
    size <- map(va[[measure]], c(1, 100))
    size[va$type == 1] <- 1
    
    if (!is.null(shading)) {
      color <- c(rep(control$itemCol[1], n_items),
        .col_picker(map(va[[shading]][va$type == 2], c(0.9, 0.1)), control$nodeCol))
    } else
      color <- c(rep(control$itemCol[1], n_items),
        .col_picker(rep(.5, sum(va$type == 2)), control$nodeCol))
    
    label <- va$label
    label[va$type == 2] <- paste(substr(class(x), 1, nchar(class(x)) - 1L), seq_along(x))
    
    nodes <- data.frame(
      id = seq_along(va$name),
      label = label,
      group = va$type,
      value = size,
      color = color,
      title = title,
      shape = ifelse(va$type == 2, "circle", "box")
    )
    
    e <- igraph::as_data_frame(g, what = "edges")
    edges <- data.frame(
      from = as.integer(factor(e$from, levels = va$name)),
      to = as.integer(factor(e$to, levels = va$name)),
      arrows = "to"
    )
    
    visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
      visNetwork::visNodes(scaling = list(label = list(enabled = TRUE))) %>%
      visNetwork::visIgraphLayout(layout = control$igraphLayout) %>%
      visNetwork::visOptions(
        highlightNearest =
          list(
            enabled = TRUE,
            degree = control$degree_highlight,
            hover = TRUE
          ),
        nodesIdSelection = control$selection_menu
      )
  }
