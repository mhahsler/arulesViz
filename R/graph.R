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


.grey_hcl <- function(level, alpha=NULL) hcl(c=0, l=level*100, 
  alpha=if(!is.null(alpha)) alpha else 1)

graph_arules <- function(rules, measure = "support", shading = "lift", 
  control=NULL, ...) {
  
  .font.family <- par()$family
  if(.font.family=="") .font.family <- "sans"
  
  control <- .get_parameters(list(
    main = paste("Graph for", length(rules), "rules"),
    nodeColors = .nodeColors(
      if(!is.null(control$alpha)) control$alpha else .5),
    node_hcl = .grey_hcl,
    edge_hcl = .grey_hcl,
    alpha = .5,
    cex = 1,
    itemLabels = TRUE,
    measureLabels = FALSE,
    precision = 3,
    type = "items",
    layout = NULL,
    layoutParams = list(),
    arrowSize = .5,
    interactive = FALSE,
    engine = "igraph",
    plot = TRUE
  ), control)
  
  opar <- par(mar = c(0,0.1,4,0.1))
  on.exit(par(opar))
  
  
  ### default layout for igraph
  if(control$interactive  || (control$engine == "igraph" 
    && is.null(control$layout))) {
    control$layout <- igraph::nicely()
  }
  
  if(control$type=="bipartite") {
    ## make bipartite graph
    leftHandSide <- as.factor(labels(lhs(rules)))
    rightHandSide <- as.factor(labels(rhs(rules)))
    names(leftHandSide) <- paste("l",as.integer(leftHandSide),sep='')
    names(rightHandSide) <- paste("r",as.integer(rightHandSide),sep='')
    e.list <- cbind(names(leftHandSide), names(rightHandSide))
    
    v <- data.frame(
      name=c(paste("l", 1:length(levels(leftHandSide)),sep=''),
        paste("r", 1:length(levels(rightHandSide)),sep='')),
      label=c(levels(leftHandSide), levels(rightHandSide)))
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v)
    
    for(m in names(quality(rules))) {
      g <- igraph::set.edge.attribute(g, m, value=quality(rules)[[m]])
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
      m <- quality(rules)[[measure]]
      e.width <- map(m, c(.5,3))
    }
    
    ## use gray value to code for measure
    e.color <- control$node_hcl(0.3, control$alpha)
    s <- NA
    if(!is.na(shading)) {
      s <- quality(rules)[[shading]]
      e.color <- control$node_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
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
        vertex.label.color="black",
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
    
  } else if(control$type=="itemsets") {
    
    itemsets <- c(lhs(rules),rhs(rules))
    itemsetLabels <- as.factor(labels(itemsets))
    v.labels <- data.frame(
      name=1:length(levels(itemsetLabels)),
      label=levels(itemsetLabels ), 
      stringsAsFactors = FALSE)
    e.list <- matrix(as.integer(itemsetLabels), nrow = length(rules))
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
    
    for(m in names(quality(rules))) {
      g <- igraph::set.edge.attribute(g, m, value=quality(rules)[[m]])
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
      m <- quality(rules)[[measure]]
      e.width <- map(m, c(.5,3))
    }
    
    e.color <- control$edge_hcl(0.3, control$alpha)
    s <- NA
    if(!is.na(shading)) {
      s <- quality(rules)[[shading]]
      e.color <- control$edge_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)
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
        att$edge$color <- control$edge_hcl(0, control$alpha)
        
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
          color <- control$edge_hcl(map(sapply(Rgraphviz::edgeData(gNEL), 
            FUN = function(i) i[[shading]]),
            c(0.9,0.1)), alpha=control$alpha)
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
          vertex.label.color="black",
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
    
  } else if(control$type=="items") {
    
    ### find used items
    itemNodes <- which(itemFrequency(items(generatingItemsets(rules)), 
      type="absolute") >0)
    
    ruleNodes <- paste("r", 1:length(rules), sep='')
    
    lhs <- LIST(lhs(rules), decode=FALSE)
    rhs <- LIST(rhs(rules), decode=FALSE)
    
    from_lhs <- unlist(lhs)
    to_lhs <- ruleNodes[rep(1:length(rules), sapply(lhs, length))]
    
    to_rhs <- unlist(rhs)
    from_rhs <- ruleNodes[rep(1:length(rules), sapply(rhs, length))]
    
    type <- c(rep(1, length(itemNodes)), rep(2, length(ruleNodes)))
    
    nodeLabels <- c(itemLabels(rules)[itemNodes], rep("", length(ruleNodes)))
    
    e.list <- cbind(c(from_lhs, from_rhs), c(to_lhs, to_rhs))
    v.labels <- data.frame(
      name=c(as.character(itemNodes), ruleNodes),
      label=nodeLabels,
      stringsAsFactors = FALSE)
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
    
    ## add quality measures
    for(m in names(quality(rules))) {
      g <- igraph::set.vertex.attribute(g, m, which(type==2)-1, 
        quality(rules)[[m]])
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
    e.color <- control$edge_hcl(.6,control$alpha)
    
    m <- NA
    if(!is.na(measure)) {
      m <- quality(rules)[[measure]]
      v.size <- c(rep(15, length(itemNodes)),
        map(m, c(5,20)))
    }
    
    s <- NA
    if(!is.na(shading)) {
      s <- quality(rules)[[shading]]
      v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
        control$node_hcl(map(s, c(0.9,0.1)), alpha=control$alpha)) 
    } else v.color <- c(rep(control$nodeColors[1], length(itemNodes)),
      control$node_hcl(rep(.5, length(rules)), alpha=control$alpha))
      
    
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
        
        gNEL <- igraph::igraph.to.graphNEL(g)
        
        if(is.null(control$layout)) control$layout <- "dot"
        att <-  Rgraphviz::getDefaultAttrs(layoutType = control$layout)
        att$edge$color <- control$edge_hcl(0, control$alpha)
        att$edge$len <- 2.0	# neato
        att$graph$rankdir <- "LR" # dot
        att$graph$ranksep <- .75 #dot
        att$graph$mclimit <- 1 #dot
        
        ## TODO: plot edge labels
        
        if(control$itemLabels) {
          nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
            fillcolor = c(rep(control$nodeColors[1], 
              length(itemNodes)), 
              control$node_hcl(map(s, c(0.9,0.1)), 
                alpha=control$alpha)), 
            label = nodeLabels,
            shape = c("box", "circle")[type], 
            width=c(rep(.75, length(itemNodes)), 
              map(m, c(0.5,1.2))), 
            fixedsize=FALSE)
        } else {
          nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
            fillcolor = c(rep(control$nodeColors[1],
              length(itemNodes)),
              control$node_hcl(map(s, c(0.9,0.1)), 
                alpha=control$alpha)),
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
          vertex.label.color="black",
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

graph_arules_is <- function(itemsets, measure = "support", shading = NULL, 
  control=NULL, ...) {
  
  .font.family <- par()$family
  if(.font.family=="") .font.family <- "sans"
  
  control <- .get_parameters(list(
    main = paste("Graph for", length(itemsets), "itemsets"),
    node_hcl = .grey_hcl,
    edge_hcl = .grey_hcl,
    #itemLabels = TRUE,	    ### not implemented yet
    #measureLabels = FALSE,
    cex = 1,
    precision = 3,
    type = "items",
    layout = NULL,
    layoutParams = list(),
    engine = "igraph",
    alpha = .5,
    arrowSize = .5,
    interactive = FALSE,
    plot = TRUE
  ), control)
  
  opar <- par(mar = c(0,0.1,4,0.1))
  on.exit(par(opar))
  
  ### default layout for igraph
  if(control$interactive  || (control$engine == "igraph" 
    && is.null(control$layout))) {
    control$layout <- igraph::nicely()
  }
  
  
  if(control$type=="items") {
    
    sets <- paste('s', as.character(1:length(itemsets)), sep='')
    items <- LIST(items(itemsets))
    e.list <- cbind(unlist(items),rep(sets, size(itemsets)))
    items <- unique(unlist(items))
    type <- c(rep(1,length(sets)), rep(2,length(items)))
    v.labels <- data.frame(
      name=c(sets,items),
      label=c(sets,items),
      stringsAsFactors = FALSE)
    
    
    v <- v.labels[,2]
    v[type==1] <- ''
    
    g <- igraph::graph.data.frame(e.list, directed=TRUE, vertices=v.labels)
    
    for(m in names(quality(itemsets))) {
      g <- igraph::set.vertex.attribute(g, m, 
        (1:length(itemsets)), value=quality(itemsets)[[m]])
    }
    
    if(!control$plot) return(g)
    
    
    v.shape <- c("circle","none")[type]
    #v.color <- control$nodeColors[type]
    v.color <- control$node_hcl(0.8, control$alpha)
    
    e.width <- 1
    e.color <- control$edge_hcl(0.3, control$alpha)
    e.label <- NA
    
    m <- NA
    if(!is.na(measure)) {
      m <- quality(itemsets)[[measure]]
      v.size <- c(map(m, c(5,20)), 
        rep(15, length(items)))
      #v.color <- c(control$node_hcl(map(m, c(0.9,0.1)), alpha=control$alpha), 
      #	    rep(NA, length(items)))
    }
    
    
    
    legend <- ''
    if(!is.na(measure))
      legend <- paste(legend, "size: ", measure[1], " (",
        paste(round(range(m),control$precision), collapse=' - '), ")\n",
        sep='')
    
    if(!control$interactive) {
      if(control$engine=="graphviz") {
        if(!.installed("Rgraphviz")) stop ("Package Rgraphviz needed. Please install from Bioconductor!")
        
        gNEL <- igraph::igraph.to.graphNEL(g)
        
        if(is.null(control$layout)) control$layout <- "dot"
        
        nAttrs <- Rgraphviz::makeNodeAttrs(gNEL, 
          fillcolor = control$nodeColors[type], 
          #    label = nodeLabels,
          shape = c("circle","box")[type], 
          width=c(c(map(m, c(.1,2))), rep(.75, length(items))), 
          fixedsize=c(TRUE, FALSE)[type])
        
        sgL <- list(list(graph = graph::subGraph(as.character(sets), gNEL), 
          cluster = TRUE,
          attrs = c(rank = "sink")))
        att <- list(graph = list(
          rankdir = "LR", #dot
          rank = "min", #dot
          ranksep = .75, #dot
          mclimit = 1 #dot
        ),
          edge=list(
            len = 2, #neato
            color = rgb(0,0,0,control$alpha)
          ))
        
        ## TODO: plot edge/item labels
        
        ## plot whines about zero length arrows
        suppressWarnings(pl <- Rgraphviz::plot(gNEL, control$layout, 
          subGList = sgL,
          nodeAttrs = nAttrs, 
          attrs = att,
          recipEdges="distinct", 
          main=control$main))
        
        text(pl@boundBox@upRight@x, pl@boundBox@upRight@y*1.05,
          legend, pos=2,cex=.8, xpd=NA)
        
        return(invisible(pl))
        
      }else{ ## igraph
 
             
        plot(g, ...,
        #  layout=control$layout(g, params=control$layoutParams),
          layout=igraph::layout_(g, control$layout),
          vertex.label.family=.font.family,
          edge.label.family=.font.family,
          vertex.shape=v.shape, 
          vertex.label=v,
          vertex.label.cex=control$cex,
          vertex.label.color="black",
          vertex.color = v.color,
          vertex.size=v.size,
          edge.width=e.width,
          edge.label=e.label,
          edge.label.cex=control$cex*.6,
          edge.arrow.size=control$arrowSize,
          main=control$main
        )
        
        mtext(legend, adj=1,cex=control$cex*.8)
      }
    }else{
      
      igraph::tkplot(g, ...,
        #layout=control$layout(g, params=control$layoutParams),
        layout=igraph::layout_(g, control$layout),
        vertex.shape=v.shape, 
        vertex.label=v,
        #vertex.label.cex=.7,
        #vertex.label.color="black",
        vertex.color = v.color,
        vertex.size=v.size,
        #edge.width=e.width,
        edge.label=e.label,
        #edge.label.cex=.5,
        edge.color=e.color,
        #main=control$main
      )
      
    }
    
    return(invisible(g))
  }
}
