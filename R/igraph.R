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
  
  x <- limit(x, control$max, shading, measure)
  
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
    check_installed(c("Rgraphviz"), action = "stop",  message = "Install package from Bioconductor at https://bioconductor.org/packages/Rgraphviz/")
    
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
