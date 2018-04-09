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


### default with alpha
.nodeColors <- function(alpha=NULL) {
  if(is.null(alpha)) alpha <- 1
  c(rgb(.4,.8,.4, alpha), rgb(.6,.6,.8, alpha))
}

graph_arules <- function(x, measure = "support", shading = "lift", 
  control=NULL, ...) {
  
  engines <- c("default", "igraph", "interactive", "graphviz", 
    "visNetwork", "htmlwidget")
  m <- pmatch(control$engine, engines, nomatch = 0)
  if(m == 0) stop("Unknown engine: ", sQuote(control$engine), 
    " Valid engines: ", paste(sQuote(engines), collapse = ", "))
  control$engine <- engines[m] 
  
  control <- c(control, list(...))
  
  ### FIXME: fix max and control
  if(pmatch(control$engine, c("visNetwork", "htmlwidget"), nomatch = 0) >0) { 
    return(visNetwork_arules(x, measure = measure, shading = shading,
      control = control))
  }
 
  ## check if shading measure is available
  if(is.null(quality(x)[[shading]])) shading <- NA
   
  if(pmatch(control$engine, c("default"), nomatch = 0) == 1) 
    control$engine <- "igraph"
  
  .font.family <- par()$family
  if(.font.family=="") .font.family <- "sans"
  
  control <- .get_parameters(control, list(
    main = paste("Graph for", min(length(x), 
      if(!is.null(control$max)) control$max else 100), class(x)),
    nodeColors = .nodeColors(
      if(!is.null(control$alpha)) control$alpha else .5),
    nodeCol = default_colors(100),
    edgeCol = grey_hcl(100),
    alpha = .5,
    cex = 1,
    itemLabels = TRUE,
    labelCol = hcl(l=0, alpha = .7),
    measureLabels = FALSE,
    precision = 3,
    layout = NULL,
    layoutParams = list(),
    arrowSize = .5,
    engine = "igraph",
    plot = TRUE,
    plot_options = list(),
    max = 100
  ))
 
  
  if(length(x) > control$max) {
    warning("plot: Too many ", class(x), " supplied. Only plotting the best ", 
      control$max, " ", class(x), " using ", sQuote(measure), 
      " (change control parameter max if needed)", call. = FALSE)
    x <- tail(x, n = control$max, by = measure, decreasing = FALSE)
  }
  
  opar <- par(mar = c(0,0.1,4,0.1))
  on.exit(par(opar))
  
  
  ### default layout for igraph
  if(control$engine == "interactive"  || (control$engine == "igraph" 
    && is.null(control$layout))) {
    control$layout <- igraph::nicely()
  }
  
  
  ## find used items
  if(class(x) == "rules") 
    itemNodes <- which(itemFrequency(items(generatingItemsets(x)), 
      type = "absolute") >0)
  else 
    itemNodes <- which(itemFrequency(items(x), 
      type = "absolute") >0)
  
  ## association nodes (rules or itemsets) 
  assocNodes <- paste("assoc", 1:length(x), sep='')
  
  ## add rhs for rules  
  if(class(x) == "rules") {
    lhs <- LIST(lhs(x), decode=FALSE)
    from_lhs <- unlist(lhs)
    to_lhs <- assocNodes[rep(1:length(x), sapply(lhs, length))]
    rhs <- LIST(rhs(x), decode=FALSE)
    to_rhs <- unlist(rhs)
    from_rhs <- assocNodes[rep(1:length(x), sapply(rhs, length))]
  } else { ### not used fro itemsets
    lhs <- LIST(items(x), decode=FALSE)
    from_lhs <- unlist(lhs)
    to_lhs <- assocNodes[rep(1:length(x), sapply(lhs, length))]
    from_rhs <- integer(0)
    to_rhs <- integer(0)
  }
  
  type <- c(rep(1, length(itemNodes)), rep(2, length(assocNodes)))
  nodeLabels <- c(itemLabels(x)[itemNodes], rep("", length(assocNodes)))
  
  e.list <- cbind(c(from_lhs, from_rhs), c(to_lhs, to_rhs))
  v.labels <- data.frame(
    name = c(as.character(itemNodes), assocNodes),
    label = nodeLabels,
    stringsAsFactors = FALSE)
  
  g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
  
  ## add quality measures
  for(m in names(quality(x))) {
    g <- igraph::set.vertex.attribute(g, m, which(type==2), 
      quality(x)[[m]])
  }
  
  if(!control$plot) return(g)
  
  ## create representation	
  v <- v.labels[,1]
  v[type==2] <- ""
  v.shape <- c("rectangle","circle")[type]
  if(control$itemLabels) {
    v <- v.labels[,2]
    v.shape <- c("none","circle")[type]
  }else{
    writeLines("items")
    print(v.labels[type==1,])
  }
  
  e.width <- 1
  e.color <- .col_picker(.6, control$edgeCol, control$alpha)
  
  m <- NA
  if(!is.na(measure)) {
    m <- quality(x)[[measure]]
    v.size <- c(rep(15, length(itemNodes)),
      map(m, c(5,20)))
  }
  
  s <- NA
  if(!is.na(shading)) {
    s <- quality(x)[[shading]]
    v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
      .col_picker(map(s, c(0.9,0.1)), control$nodeCol, 
        alpha=control$alpha)) 
  } else v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
    .col_picker(rep(.5, length(x)), control$nodeCol, 
      alpha=control$alpha))
  
  
  if(control$measureLabels) {
    if(is.na(m) || is.na(s)) {
      if(!is.na(m)) e.label <- round(m, control$precision)
      if(!is.na(s)) e.label <- round(s, control$precision)
    }else{
      v[type==2] <- paste(round(m, control$precision),"\n", 
        round(s, control$precision), sep='')
    }
  }
  
  
  legend <- ''
  if(!is.na(measure[1])) legend <- paste(legend,
    "size: ", measure[1], " (",
    paste(round(range(m),control$precision), collapse=' - '), ")\n",
    sep ='')
  
  if(!is.na(shading)) legend <- paste(legend,
    "color: ", shading, " (",
    paste(round(range(s),control$precision), collapse=' - '), ")",
    sep ='')
  
  if(control$engine=="graphviz") {
    if(!.installed("Rgraphviz")) stop ("Package Rgraphviz needed!")
    
    requireNamespace("Rgraphviz") 
    gNEL <- igraph::igraph.to.graphNEL(g)
    
    if(is.null(control$layout)) control$layout <- "dot"
    att <-  Rgraphviz::getDefaultAttrs(layoutType = control$layout)
    att$edge$color <- .col_picker(0, control$edgeCol, control$alpha)
    att$edge$len <- 2.0	# neato
    att$graph$rankdir <- "LR" # dot
    att$graph$ranksep <- .75 #dot
    att$graph$mclimit <- 1 #dot
    
    ## TODO: plot edge labels
    s <- NA
    if(!is.na(shading)) {
      s <- quality(x)[[shading]]
      v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
        .col_picker(map(s, c(0.9,0.1)), control$nodeCol, 
          alpha=control$alpha)) 
    } else v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
      .col_picker(rep(.5, length(x)), control$nodeCol, 
        alpha=control$alpha))
    
    if(control$itemLabels) {
      nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
        fillcolor = v.color, 
        label = nodeLabels,
        shape = c("box", "circle")[type], 
        width=c(rep(.75, length(itemNodes)), 
          map(m, c(0.5,1.2))), 
        fixedsize=FALSE)
    } else {
      nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
        fillcolor = v.color,
        shape = c("box", "circle")[type],
        width=c(rep(.75, length(itemNodes)),
          map(m, c(0.5,1.2))),
        fixedsize=FALSE
      )
      #writeLines("Itemsets")
      #print(levels(itemsetLabels))
    }
    
    ## plot whines about zero length arrows
    suppressWarnings(pl <- Rgraphviz::plot(gNEL, control$layout, 
      nodeAttrs = nAttrs,
      attrs = att,
      recipEdges="distinct", 
      main=control$main))
    
    text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05,
      legend, pos=2,cex=.8, xpd=NA)
    
    return(invisible(pl))
  }
  
  if(control$engine=="igraph") {
    do.call(plot, c(list(g,
      #layout=control$layout(g, params=control$layoutParams), 
      layout=igraph::layout_(g, control$layout), 
      vertex.label.family=.font.family,
      edge.label.family=.font.family,
      vertex.shape=v.shape, 
      vertex.label=v,
      vertex.label.cex=control$cex,
      #vertex.label.color="black",
      vertex.label.color=control$labelCol,
      vertex.color = v.color,
      vertex.size=v.size,
      edge.width=e.width,
      #edge.label=e.label,
      edge.label.cex=control$cex*.6,
      edge.color=e.color,
      edge.arrow.size=control$arrowSize,
      main=control$main), control$plot_options)
    )
    
    mtext(legend, adj=1,cex=control$cex*.8)
  }
  
  if(control$engine=="interactive") {
    do.call(igraph::tkplot, c(list(g, 
      #layout=control$layout(g, params=control$layoutParams),
      layout=igraph::layout_(g, control$layout),
      #vertex.shape=v.shape, 
      vertex.label=v,
      #vertex.label.cex=.7,
      #vertex.label.color="black",
      vertex.color = v.color,
      vertex.size=v.size,
      #edge.width=e.width,
      #edge.label=e.label,
      #edge.label.cex=.5,
      edge.color=e.color
      #main=control$main
    ), control$plot_options))
  }
  
  return(invisible(g))
}

