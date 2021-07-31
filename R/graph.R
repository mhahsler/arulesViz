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
    edges_per_assoc <- sapply(items, length) 
    edges <- t(matrix(unlist(lapply(items, utils::combn, 2)), nrow = 2))
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

graph_igraph <- function(x,
  measure = "support",
  shading = "lift",
  control = NULL,
  ...) {
  
  control <- c(control, list(...))
  if (pmatch(control$engine, c("default"), nomatch = 0) == 1)
    control$engine <- "igraph"
  
  .font.family <- par()$family
  if (.font.family == "")
    .font.family <- "sans"
  
  control <- .get_parameters(
    control,
    list(
      main = paste("Graph for", min(length(x),
        if (!is.null(control$max))
          control$max
        else
          100), class(x)),
      max = 100,
      # maximum number or rules/itemsets
      
      nodeCol = default_colors(100),
      itemnodeCol = .nodeColors()[1],
      edgeCol = grDevices::hcl(l = 70, c = 0, alpha = 1),
      labelCol = grDevices::hcl(l = 0, c = 0, alpha = .7),
      
      # itemLabels = TRUE,
      measureLabels = FALSE,
      precision = 3,
      
      arrowSize = .5,
      alpha = .5,
      cex = 1,
      
      layout = NULL,
      layoutParams = list(),
      
      engine = "igraph",
      plot = TRUE,
      plot_options = list()
    )
  )
  
  
  if (length(x) > control$max) {
    warning(
      "plot: Too many ",
      class(x),
      " supplied. Only plotting the best ",
      control$max,
      " ",
      class(x),
      " using ",
      sQuote(measure),
      " (change control parameter max if needed)",
      call. = FALSE
    )
    x <- tail(x,
      n = control$max,
      by = measure,
      decreasing = FALSE)
  }
  
  opar <- par(mar = c(0, 0.1, 4, 0.1))
  on.exit(par(opar))
  
  
  ### default layout for igraph
  if (control$engine == "interactive"  ||
      (control$engine == "igraph"
        && is.null(control$layout))) {
    control$layout <- igraph::nicely()
  }
  
  g <- associations2igraph(x)
  if (!control$plot)
    return(g)
  
  ## create representation
  vs <- igraph::V(g)
  #v <- vs$label
  #v.shape <- c("rectangle","circle")[vs$type]
  #if(control$itemLabels) {
  v <- vs$label
  v.shape <- c("none", "circle")[vs$type]
  #}
  
  e.width <- 1
  e.color <- control$edgeCol
  
  m <- NA
  nItemNodes = sum(vs$type == 1)
  if (!is.na(measure[1])) {
    m <- quality(x)[[measure[1]]]
    v.size <-
      c(rep(15, nItemNodes),   ### item nodes have no measure
        map(m, c(5, 20)))
  }
  
  s <- NA
  if (!is.null(shading)) {
    s <- quality(x)[[shading]]
    v.color <- c(
      rep(control$itemnodeCol, nItemNodes),
      .col_picker(map(s, c(0.9, 0.1)), control$nodeCol,
        alpha = control$alpha)
    )
  } else
    v.color <- c(
      rep(control$itemnodeCol, nItemNodes),
      .col_picker(rep(.5, length(x)), control$nodeCol,
        alpha = control$alpha)
    )
  
  
  if (control$measureLabels &&
      (!is.na(measure[1]) || !is.null(shading))) {
    if (!is.na(measure[1]) && !is.null(shading))
      mlabs <- paste(round(m, control$precision),
        round(s, control$precision),
        sep = "\n")
    else {
      if (!is.na(measure[1]))
        mlabs <- round(m, control$precision)
      if (!is.null(shading))
        mlabs <- round(s, control$precision)
    }
    v[vs$type == 2] <- mlabs
  }
  
  # Legend
  legend <- ''
  if (!is.na(measure[1]))
    legend <- paste(legend,
      "size: ",
      measure[1],
      " (",
      paste(round(range(m), control$precision), collapse = ' - '),
      ")\n",
      sep = '')
  
  if (!is.null(shading))
    legend <- paste(legend,
      "color: ",
      shading,
      " (",
      paste(round(range(s), control$precision), collapse = ' - '),
      ")",
      sep = '')
  
  if (control$engine == "graphviz") {
    check_installed(c("Rgraphviz"), action = "stop",  message = "Install package from Bioconductor at https://www.bioconductor.org/packages/release/bioc/html/Rgraphviz.html")
    
    gNEL <- igraph::igraph.to.graphNEL(g)
    
    if (is.null(control$layout))
      control$layout <- "dot"
    att <-  Rgraphviz::getDefaultAttrs(layoutType = control$layout)
    att$edge$color <- control$edgeCol
    att$edge$len <- 2.0	# neato
    att$graph$rankdir <- "LR" # dot
    att$graph$ranksep <- .75 #dot
    att$graph$mclimit <- 1 #dot
    
    ## TODO: plot edge labels
    s <- NA
    if (!is.null(shading)) {
      s <- quality(x)[[shading]]
      v.color <- c(
        rep(control$itemnodeCol, nItemNodes),
        .col_picker(map(s, c(0.9, 0.1)), control$nodeCol,
          alpha = control$alpha)
      )
    } else
      v.color <- c(
        rep(control$itemnodeCol, nItemNodes),
        .col_picker(rep(.5, length(x)), control$nodeCol,
          alpha = control$alpha)
      )
    
    #    if(control$itemLabels) {
    nAttrs <- Rgraphviz::makeNodeAttrs(
      gNEL,
      fillcolor = v.color,
      label = vs$label,
      shape = c("box", "circle")[vs$type],
      width = c(rep(.75, nItemNodes),
        map(m, c(0.5, 1.2))),
      fixedsize = FALSE
    )
    #    } else {
    #      nAttrs <- Rgraphviz::makeNodeAttrs(gNEL,
    #        fillcolor = v.color,
    #        shape = c("box", "circle")[vs$type],
    #        width=c(rep(.75, nItemNodes),
    #          map(m, c(0.5,1.2))),
    #        fixedsize=FALSE
    #      )
    #writeLines("Itemsets")
    #print(levels(itemsetLabels))
    #    }
    
    ## plot whines about zero length arrows
    suppressWarnings(
      pl <- Rgraphviz::plot(
        gNEL,
        control$layout,
        nodeAttrs = nAttrs,
        attrs = att,
        recipEdges = "distinct",
        main = control$main
      )
    )
    
    text(
      pl@boundBox@upRight@x,
      pl@boundBox@upRight@y * 1.05,
      legend,
      pos = 2,
      cex = .8,
      xpd = NA
    )
    
    return(invisible(pl))
  }
  
  if (control$engine == "igraph") {
    do.call(plot, c(
      control$plot_options,
      list(
        g,
        #layout=control$layout(g, params=control$layoutParams),
        layout = igraph::layout_(g, control$layout),
        vertex.label.family = .font.family,
        edge.label.family = .font.family,
        vertex.shape = v.shape,
        vertex.label = v,
        vertex.label.cex = control$cex,
        #vertex.label.color="black",
        vertex.label.color = control$labelCol,
        vertex.color = v.color,
        vertex.size = v.size,
        edge.width = e.width,
        #edge.label=e.label,
        edge.label.cex = control$cex * .6,
        edge.color = e.color,
        edge.arrow.size = control$arrowSize,
        main = control$main
      )
    ))
    
    mtext(legend, adj = 1, cex = control$cex * .8)
  }
  
  if (control$engine == "interactive") {
    do.call(igraph::tkplot, c(
      control$plot_options,
      list(
        g,
        #layout=control$layout(g, params=control$layoutParams),
        layout = igraph::layout_(g, control$layout),
        #vertex.shape=v.shape,
        vertex.label = v,
        #vertex.label.cex=.7,
        #vertex.label.color="black",
        vertex.color = v.color,
        vertex.size = v.size,
        #edge.width=e.width,
        #edge.label=e.label,
        #edge.label.cex=.5,
        edge.color = e.color
        #main=control$main
      )
    ))
  }
  
  return(invisible(g))
}
