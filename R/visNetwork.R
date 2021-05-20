#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
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

graph_visNetwork <- function(x, measure = "support", shading = "lift", 
  control=NULL, ...) {
  
  if(class(x) != "rules") stop("Only implemented for rules!")
  
  control <- c(control, list(...))  
  
  control <- .get_parameters(control, list(
    #main = paste("Graph for", length(x), "rules"),
    itemCol = grDevices::hcl(h = 260),
    nodeCol = default_colors(100, alpha = 0),
    precision = 3,
    igraphLayout = "layout_nicely",
    interactive = TRUE,
    engine = "visNetwork", 
    max = 100,
    selection_menu = TRUE,
    degree_highlight =1
  ))
  
  if(length(x) > control$max) {
    warning("Too many rules supplied. Only plotting the best ", 
      control$max, " rules using ", shading, 
      " (change control parameter max if needed)", call. = FALSE)
    x <- tail(x, n = control$max, by = shading, decreasing = FALSE)
  }
  
  itemNodes <- which(itemFrequency(items(generatingItemsets(x)), 
    type="absolute") >0)
  
  lhs <- LIST(lhs(x), decode=FALSE)
  rhs <- LIST(rhs(x), decode=FALSE)
  
  itemNodes <- unique(c(unlist(lhs), unlist(rhs)))
  ruleNodes <- paste("r", 1:length(x), sep='')
  
  nodeLabels <- c(itemLabels(x)[itemNodes], paste("rule", 1:length(ruleNodes)))
  
  allNodes <- factor(c(itemNodes, ruleNodes), levels = c(itemNodes, ruleNodes))
  nodeType <- c(rep("item", length(itemNodes)), rep("rule", length(ruleNodes)))
  
  from_lhs <- match(unlist(lhs), allNodes)
  to_lhs <- rep(1:length(x), sapply(lhs, length)) + length(itemNodes)
  
  to_rhs <- match(unlist(rhs), allNodes)
  from_rhs <- rep(1:length(x), sapply(rhs, length)) + length(itemNodes)
  
  titleRules <- paste0('<B>[',1:length(x),']</B><BR>',
    labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
      ruleSep = '<BR>&nbsp;&nbsp; => ', 
      setStart = '<B>{', setEnd = '}</B>'),
    "<BR><BR>", 
    apply(quality(x), MARGIN = 1, 
      FUN = function(x) paste(names(x), "=", signif(x,control$precision), collapse = "<BR>")))
  
  title <- c(itemLabels(x)[itemNodes], titleRules)
  
  s <- quality(x)[[measure]]
  size <- rep(1, length(nodeType))
  size[nodeType == "rule"] <- map(s, c(1, 100)) 
  
  if(!is.na(shading)) {
    s <- quality(x)[[shading]]
    color <- c(rep(control$itemCol[1], length(itemNodes)),
      .col_picker(map(s, c(0.9,0.1)), control$nodeCol)) 
  } else color <- c(rep(control$itemCol[1], length(itemNodes)),
    .col_picker(rep(.5, length(x)), control$nodeCol)) 

  nodes <- data.frame(
    id = as.integer(allNodes), 
    label = nodeLabels, 
    group = nodeType,
    value = size,
    color = color,
    title = title,
    shape = ifelse(nodeType == "rule", "circle", "box")
  )
  
  edges <- data.frame(
    from = c(from_lhs, from_rhs), 
    to = c(to_lhs, to_rhs), 
    arrows = "to") 
  
  visNetwork::visNetwork(nodes = nodes, edges = edges) %>% 
    visNetwork::visNodes(scaling = list(label = list(enabled = TRUE))) %>%
    visNetwork::visIgraphLayout(layout = control$igraphLayout) %>%
    visNetwork::visOptions(highlightNearest = 
        list(enabled = TRUE, degree = control$degree_highlight, hover = TRUE), 
      nodesIdSelection = control$selection_menu
      )
}
