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
  
  ### FIXME: fix max and control
  if(pmatch(control$engine, c("visNetwork", "htmlwidget"), nomatch = 0) >0) { 
    return(visNetwork_arules(x, measure = measure, shading = shading,
      control = control, ...))
  }
 
  ## check if shading measure is available
  if(is.null(quality(x)[[shading]])) shading <- NA
   
  if(pmatch(control$engine, c("default", "igraph", "graphviz"), 
    nomatch = 0) == 0)  
    stop("Unknown engine for graph: '", control$engine, 
      "'\nValid engines: 'default' (same as 'igraph'), 'htmlwidget' (same as 'visNetwork'), 'graphviz'.")
  
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
    type = "items",
    layout = NULL,
    layoutParams = list(),
    arrowSize = .5,
    interactive = FALSE,
    engine = "igraph",
    plot = TRUE,
    max = 100
  ))
 
  
  if(length(x) > control$max) {
    warning("too many ", class(x), " supplied only plotting the best ", 
      control$max, " ", class(x), " using ", sQuote(measure), 
      " (change control parameter max if needed)")
    x <- tail(x, n = control$max, by = measure, decreasing = FALSE)
  }
    
  opar <- par(mar = c(0,0.1,4,0.1))
  on.exit(par(opar))
  
  
  ### default layout for igraph
  if(control$interactive  || (control$engine == "igraph" 
    && is.null(control$layout))) {
    control$layout <- igraph::nicely()
  }
 
  ## bipartite graph for rules 
  if(control$type=="bipartite") {
    if(class(x) != "rules") stop("type bipartite only available for rules!")
    
    ## make bipartite graph
    leftHandSide <- as.factor(labels(lhs(x)))
    rightHandSide <- as.factor(labels(rhs(x)))
    names(leftHandSide) <- paste("l",as.integer(leftHandSide), sep='')
    names(rightHandSide) <- paste("r",as.integer(rightHandSide), sep='')
    e.list <- cbind(names(leftHandSide), names(rightHandSide))
    
    v <- data.frame(
      name=c(paste("l", 1:length(levels(leftHandSide)), sep=''),
        paste("r", 1:length(levels(rightHandSide)), sep='')),
      label=c(levels(leftHandSide), levels(rightHandSide)))
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v)
    
    for(m in names(quality(x))) {
      g <- igraph::set.edge.attribute(g, m, value=quality(x)[[m]])
    }
    
    if(!control$plot) return(g)
    
    v <- labels(g)
    v.shape <- "circle"
    if(control$itemLabels) {
      v <- c(levels(leftHandSide), levels(rightHandSide))
      v.shape <- "none"
    }else{
      writeLines("LHS")
      print(levels(leftHandSide))
      writeLines("RHS")
      print(levels(leftHandSide))
    }
    
    
    e.width <- 1
    m <- NA
    if(!is.na(measure)) {
      m <- quality(x)[[measure]]
      e.width <- map(m, c(.5,3))
    }
    
    ## use gray value to code for measure
    e.color <- .col_picker(.3, control$nodeCol, control$alpha)
    s <- NA
    if(!is.na(shading)) {
      s <- quality(x)[[shading]]
      e.color <- .col_picker(map(s, c(0.9,0.1)), control$nodeCol, 
        alpha=control$alpha)
    }
    
    e.label <- NA
    if(control$measureLabels) {
      if(is.na(m) || is.na(s)) {
        if(!is.na(m)) e.label <- round(m, control$precision)
        if(!is.na(s)) e.label <- round(s, control$precision)
      }else{
        e.label <- paste(round(m, control$precision),"/", 
          round(s, control$precision), sep='')
      }
    }
    
    
    legend <- ''
    if(!is.na(measure))
      legend <- paste(legend, "width: ", measure[1], " (",
        paste(round(range(m),control$precision), collapse=' - '), ")\n",
        sep='')
    
    if(!is.na(shading)) legend <- paste(legend,
      "color: ", shading, " (",
      paste(round(range(s), control$precision), collapse=' - '), ")",
      sep ='')
    
    v.color <-  .nodeColors(.5)[c( 
      rep(1, length(levels(leftHandSide))),
      rep(2, length(levels(rightHandSide))))]
    
    if(!control$interactive) {
      
      # igraph
      plot(g, ...,
        #layout=control$layout(g, params=control$layoutParams),
        layout=igraph::layout_(g, control$layout),
        vertex.label.family=.font.family,
        edge.label.family=.font.family,
        vertex.shape=v.shape, 
        vertex.label=v,
        vertex.label.cex=control$cex,
        vertex.label.color=control$labelCol,
        vertex.color = v.color,
        #vertex.size=v.size,
        edge.width=e.width,
        edge.label=e.label,
        edge.label.cex=control$cex*.6,
        edge.color=e.color,
        edge.arrow.size=control$arrowSize,
        main=control$main
      )
      mtext(legend, adj=1,cex=control$cex*.8)
      
    }else {
      igraph::tkplot(g, ...,
        #layout=control$layout(g, params=control$layoutParams),
        layout=igraph::layout_(g, control$layout),
        #vertex.shape=v.shape, 
        vertex.label=v,
        #vertex.label.cex=.7,
        #vertex.label.color="black",
        vertex.color = v.color,
        #vertex.size=v.size,
        #edge.width=e.width,
        edge.label=e.label,
        #edge.label.cex=.5,
        edge.color=e.color,
        #main=control$main
      )
    }
    
    return(invisible(g))
    
  ## plot itemsets for rules
  } else if(control$type=="itemsets") {
    if(class(x) != "rules") stop("type itemsets only available for rules!")
    
    itemsets <- c(lhs(x), rhs(x))
    itemsetLabels <- as.factor(labels(itemsets))
    v.labels <- data.frame(
      name=1:length(levels(itemsetLabels)),
      label=levels(itemsetLabels ), 
      stringsAsFactors = FALSE)
    e.list <- matrix(as.integer(itemsetLabels), nrow = length(x))
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
    
    for(m in names(quality(x))) {
      g <- igraph::set.edge.attribute(g, m, value=quality(x)[[m]])
    }
    
    if(!control$plot) return(g)
    
    v <- v.labels[,1]
    v.shape <- "circle"
    if(control$itemLabels) {
      v <- v.labels[,2]
      v.shape <- "none"
    }else{
      writeLines("itemsets")
      print(v.labels[,2])
    }
    
    
    ## use gray value to code for measure
    #color <- paste("grey", floor(map(m, c(80,1))), sep='')
    e.width <- 1
    m <- NA
    if(!is.na(measure)) {
      m <- quality(x)[[measure]]
      e.width <- map(m, c(.5,3))
    }
    
    e.color <- .col_picker(.3, control$edgeCol, control$alpha)
    s <- NA
    if(!is.na(shading)) {
      s <- quality(x)[[shading]]
      e.color <- .col_picker(map(s, c(0.9,0.1)), control$edgeCol, 
        alpha=control$alpha)
    }
    
    e.label <- NA
    if(control$measureLabels) {
      if(is.na(m) || is.na(s)) {
        if(!is.na(m)) e.label <- round(m, control$precision)
        if(!is.na(s)) e.label <- round(s, control$precision)
      }else{
        e.label <- paste(round(m, control$precision),"/", 
          round(s, control$precision), sep='')
      }
    }
    
    
    legend <- ''
    if(!is.na(measure))
      legend <- paste(legend, "width: ", measure[1], " (",
        paste(round(range(m),control$precision), collapse=' - '), ")\n",
        sep='')
    
    if(!is.na(shading)) legend <- paste(legend,
      "color: ", shading, " (",
      paste(round(range(s),control$precision), collapse=' - '), ")",
      sep ='')
    
    v.size <- 15 # default
    #v.color <- NA   
    
    if(!control$interactive) {
      if(control$engine=="graphviz") {
        if(!.installed("Rgraphviz")) stop ("Package Rgraphviz needed. Please install from Bioconductor!")
       
        requireNamespace("Rgraphviz") 
        gNEL <- igraph::igraph.to.graphNEL(g)
        
        if(is.null(control$layout)) control$layout <- "neato"
        att <-  Rgraphviz::getDefaultAttrs(layoutType = control$layout)
        #att$node$width <- .75
        #att$node$fillcolor <- "#e0e0e0"
        #att$node$shape <- "ellipse"
        #att$node$fixedsize <- FALSE
        #att$node$fontsize <- 8.0
        #att$edge$weight <- 4.0
        att$edge$len <- 2.5 # neato
        att$edge$color <- .col_picker(0, control$edgeCol, control$alpha)
        
        ## edges
        eAttrs <- list()
        if(control$measureLabels) {
          edgeLabels <- paste(paste(round(m, control$precision), 
            "/", round(s, control$precision)))
          edgeLabels <- edgeLabels[setdiff(seq(along = edgeLabels),
            Rgraphviz::removedEdges(gNEL))]
          names(edgeLabels) <- Rgraphviz::edgeNames(gNEL)
          eAttrs$label <- edgeLabels
          #labelfontsize <- rep("1", length(edgeNames(graph)))
          #names(labelfontsize) <- edgeNames(graph)
          #eAttrs$labelfontsize <- labelfontsize
        }
        
        ## use gray value to code for measure
        #color <- paste("grey", floor(map(m, c(80,1))), sep='')
        if(!is.na(measure)) {
          lwd <- map(sapply(Rgraphviz::edgeData(gNEL), 
            FUN = function(i) i[[measure]]),
            c(2,5))
          names(lwd) <- Rgraphviz::edgeNames(gNEL)
          eAttrs$lwd <- lwd
        }
        
        if(!is.na(shading)) {
          color <- .col_picker(map(sapply(Rgraphviz::edgeData(gNEL), 
            FUN = function(i) i[[shading]]),
            c(0.9,0.1)), control$edgeCol, alpha=control$alpha)
          names(color) <- Rgraphviz::edgeNames(gNEL)
          eAttrs$color <- color
        }
        
        
        if(control$itemLabels) {
          nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
            fillcolor = control$nodeColors[1], 
            label = levels(itemsetLabels)[as.integer(graph::nodes(gNEL))],
            shape = "box", width=0.75, fixedsize=FALSE)
        } else {
          nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
            fillcolor = control$nodeColors[1],
            shape = "circle")
          
          # writeLines("Itemsets")
          #  print(levels(itemsetLabels))
        }
        
        ## plot whines about zero length arrows
        suppressWarnings(pl <- Rgraphviz::plot(gNEL, control$layout, 
          edgeAttrs = eAttrs,
          nodeAttrs = nAttrs,
          attrs = att,
          recipEdges="distinct", 
          main=control$main))
        
        ## legend
        text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05,
          legend, pos=2,cex=.8, xpd=NA)
        
        return(invisible(pl))
        
      }else{ ## igraph
        plot(g, ..., 
          #layout=control$layout(g, params=control$layoutParams),
          layout=igraph::layout_(g, control$layout),
          vertex.label.family=.font.family,
          edge.label.family=.font.family,
          vertex.shape=v.shape, 
          vertex.label=v,
          vertex.label.cex=control$cex,
          #vertex.label.color="black",
          vertex.label.color=control$labelCol,
          #vertex.color = v.color,
          #vertex.size=v.size,
          edge.width=e.width,
          edge.label=e.label,
          edge.label.cex=control$cex*.6,
          edge.color=e.color,
          edge.arrow.size=control$arrowSize,
          main=control$main
        )
        
        mtext(legend, adj=1,cex=control$cex*.8)
        
      }
    }else {
      
      igraph::tkplot(g, ..., 
        #layout=control$layout(g, params=control$layoutParams),
        layout=igraph::layout_(g, control$layout),
        #vertex.shape=v.shape, 
        vertex.label=v,
        #vertex.label.cex=.7,
        #vertex.label.color="black",
        #vertex.color = v.color,
        vertex.size=v.size,
        #edge.width=e.width,
        edge.label=e.label,
        #edge.label.cex=.5,
        edge.color=e.color,
        #main=control$main
      )
    }
    return(invisible(g))
    
    
  ## this is the default plot for itemsets and rules  
  } else if(control$type=="items") {
    
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
      g <- igraph::set.vertex.attribute(g, m, which(type==2)-1, 
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
    
    if(!control$interactive) {
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
        
      }else{ ## igraph
        plot(g, ...,
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
          main=control$main
        )
        
        mtext(legend, adj=1,cex=control$cex*.8)
      }
    }else {
      igraph::tkplot(g, ..., 
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
        edge.color=e.color,
        #main=control$main
      )
    }
    
    return(invisible(g))
    
  } else stop("Unknown plot type.")
  
}

