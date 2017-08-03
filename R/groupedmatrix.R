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


grouped_matrix_arules <- function(rules, measure, shading, control=NULL, ...){
  
  ## measure controls circle size
  ## shading controls color
  
  control <- .get_parameters(control, list(
    main = paste("Grouped Matrix for", length(rules), "Rules"),
    k = 20,
    rhs_max = 10,
    lhs_items = 2,
    aggr.fun=median, 
    ## fix lift so serveral plots are comparable (NA: take max)
    col = default_colors(100),
    reverse = TRUE, 
    xlab = NULL, 
    ylab = NULL, 
    legend = paste("Size:", measure, "\nColor:", shading),
    spacing = -1, 
    panel.function = panel.circles, 
    gp_main   = gpar(cex=1.2, fontface="bold"),
    gp_labels = gpar(cex=.8), 
    gp_labs   = gpar(cex=1.2, fontface="bold"),
    gp_lines  = gpar(col="gray", lty=3),
    newpage=TRUE,
    interactive = FALSE,
    max.shading=NA,
    engine = "default"
  ))
  
  if(pmatch(control$engine, c("default"), nomatch = 0) == 0)  
    stop("Unknown engine for grouped matrix plot: '", control$engine, 
      "' - Valid engine: 'default'.")
  
  x <- grouped_matrix_int(rules, measure, shading, control) 
  
  if(!control$interactive) return(invisible(x))
  
  ## interactive mode
  cat("Interactive mode.\n")
  
  ## fix max.shading
  control$max.shading <- x$max.shading
  
  seekViewport("grouped_matrix")
  
  ## draw buttons
  gI <- gInteraction(data.frame(
    row.names = c("inspect","zoom in", "zoom out", "end"),
    active = rep(FALSE, 4),
    x = c(0.3, 0.5, 0.7, 0.9),
    y = I(rep(unit(0, "lines"), 4)),
    #y = I(rep(unit(-3, "lines"), 4)),
    w = I(rep(unit(3.5, "lines"), 4)),
    h = I(rep(unit(1, "lines"), 4))
  )
  )
  
  drawButtons(gI)
  
  ## event loop
  while(TRUE) {
    gI <- gGetEvent(gI, box=FALSE, checkPlotBoundaries=FALSE)
    
    b <- lastButton(gI)
    if(is.null(b)) next
    
    ## actions
    if(b=="end") {
      cat("Leaving interactive mode.\n")
      return(rules)
    }
    
    if(b=="zoom out") {
      cat("Going up.\n")
      return("zoom out")
    }
    
    
    select <- convertLoc(selection(gI)$loc,
      "native", valueOnly=TRUE)$x
    if(is.null(select)) {
      cat("Select a LHS first!\n")
      gI <- resetButtons(gI)
      next
    }
    
    select <- round(select) 
    
    if(select>0 && select <= control$k) {
      select <- get_order(x$order[2])[select]
      rulesSelected <- rules[x$cl==select]
    }else{
      cat("Illegal selection! Choose a LHS.\n")
      next
    }
    
    if(b=="zoom in") {
      if(length(unique(lhs(rulesSelected)))<2) {
        cat("Can't zoom in any further. Use inspect!\n")
        gI <- changeButton(gI, "zoom in", FALSE)
        next
      }
      
      cat("Zooming in. This might take a while\n")
      
      ret <- grouped_matrix_arules(rulesSelected, measure, 
        shading, control, ...)
      
      if(!identical(ret, "zoom out")) return(ret)
      
      ## we come back up so replot
      plot(x, control=control)
      seekViewport("grouped_matrix")
      gI <- resetButtons(gI)
    }
    
    if(b=="inspect") {
      cat("Selected rules:\n")
      ## FIXME: click on bubble
      #selectRHS <- round(as.numeric(convertY(location$Y, "native")))
      inspect(sort(rulesSelected, by="lift"))
      gI <- changeButton(gI, "inspect", FALSE)
    }
    
    ## nothing else to do
    next
  }
}

## helper
rowMaxs <- function(x, na.rm=FALSE) apply(x, MARGIN=1, max, na.rm=na.rm)

.aggr <- function(m, cl, aggr.fun = median) {
  ma <- matrix(nrow=nrow(m), ncol=0)
  for(i in 1:max(cl)) {
    ma <- cbind(ma, apply(m[, cl==i, drop=FALSE], 
      MARGIN=1, aggr.fun , na.rm=TRUE))
  }
  ma[!is.finite(ma)] <- NA
  ma
}

## create an grouped_matrix
grouped_matrix_int <- function(rules, measure, shading, control) {
  k <- control$k
  aggr.fun <- control$aggr.fun
  max.shading <- control$max.shading
  col <- control$col
  
  ## check k
  if(length(unique(lhs(rules)))< k) k <- length(unique(lhs(rules)))
  
  ## cluster for shading
  s <- rulesAsMatrix(rules, shading)
  if(is.na(max.shading)) max.shading <- max(s, na.rm=TRUE)
  
  ## FIXME: this handling of na for clustering is not great!
  s_clust <- s
  if(shading=="lift") naVal <- 1
  else naVal <- 0
  s_clust[is.na(s_clust)] <- naVal
  
  s_clust <- t(s_clust)
  
  ## are there enought non-identical points for k-means?
  ## if not then we group the identical ones using hclust
  if(sum(!duplicated(s_clust))>k) 
      km <- kmeans(s_clust, k, iter.max=50, nstart=10)$cl
  else 
      km <- cutree(hclust(dist(s_clust)), h=0)
  
  sAggr <- .aggr(s, km, aggr.fun)
  
  ## reorder for shading
  order <- ser_permutation(
    order(apply(sAggr, MARGIN=1, FUN=aggr.fun, na.rm=TRUE), 
      decreasing=TRUE),
    order(apply(sAggr, MARGIN=2, FUN=aggr.fun, na.rm=TRUE), 
      decreasing=TRUE)
  )
  
  cl <- vector()
  enc <- attr(s, "encoding")
  for(i in 1:ncol(enc)) cl[enc[,i]] <- km[i]
  
  ## use measure for size
  mAggr <- .aggr(rulesAsMatrix(rules, measure[1]), km, aggr.fun)
  
  ret <- list(rules=rules, measure=measure, shading=shading, 
    cl=cl, km= km, lhs_items = control$lhs_items, max.shading=max.shading, 
    aggr.fun=aggr.fun, 
    order=order, k=k, sAggr=sAggr, mAggr=mAggr)
  class(ret) <- "grouped_matrix"
  
  ## call plotting work horse
  plot(ret, control=control)
  
  ret
}

## display grouped_matrix
plot.grouped_matrix <- function(x, ...) {
  control <- list(...)$control
  
  col <- control$col
  
  ## circle size
  sn <- x$mAggr
  ## shading
  ln <- x$sAggr
  
  ## get most important item in the lhs
  f <- lapply(split(x$rules, x$cl), FUN = function(r) itemFrequency(lhs(r), 
    type = "absolute"))
  ## divide by sum to find most important item...
  f <- lapply(f, "/", itemFrequency(lhs(x$rules), type = "absolute")+1L)
 
  most_imp_item <- lapply(f, FUN = 
      function(x) {
        items <- sum(x>0)
        if(items==0) { "" }
        else if(control$lhs_items<1){ 
          paste(items, "items") 
        } else if(items>control$lhs_items){
          paste(paste(names(x[head(order(x, decreasing = TRUE), n = control$lhs_items)]), 
            collapse = ", "), ", +", items-control$lhs_items, " items", sep="")
        }else{
          paste(names(x[head(order(x, decreasing = TRUE), n = control$lhs_items)]), 
            collapse = ", ")
        }
      })
 
  control$ylab <- paste(
    paste(format(table(x$cl)), " rules: ", '{',most_imp_item, '}', sep=''))
      
  grouped_matrix_plot_int(
    x = map(sn, c(0.2,1)), 
    y = map(ln, range = c(1,.2), 
      from.range = c(min(x$sAggr, na.rm=TRUE), x$max.shading)),
    order = x$order,
    options = control
  )
}

## inspect rules inside an grouped_matrix
inspect.grouped_matrix <- function(x, cluster, measure="lift") {
  inspect(sort(x$rules[x$cl==cluster], by=measure))
}


## workhorse for plotting
## based on bertinplot in package seriation
grouped_matrix_plot_int <- function(x, y, order = NULL, options = NULL) {
  if (!is.matrix(x)) 
    stop("Argument 'x' must be a matrix.")

  if (!is.null(options$xlab)) rownames(x) <- options$xlab
  if (!is.null(options$ylab)) colnames(x) <- options$ylab
  
  if (!is.null(order)) {
    x <- permute(x, order)
    y <- permute(y, order)
  }
  
  ### only show the top RHS
  suppressed_rhs <- 0
  if(!is.null(options$rhs_max) && 
      options$rhs_max > 0 && 
      nrow(x) > options$rhs_max) {
    suppressed_rhs <- nrow(x) - options$rhs_max
    x <- x[1:options$rhs_max, , drop=FALSE]
    y <- y[1:options$rhs_max, , drop = FALSE]
  }
  
  if (options$reverse) {
    x <- t(x)
    y <- t(y)
    tmp <- options$xlab
    options$xlab <- options$ylab
    options$ylab <- tmp
    order <- rev(order)
  }
  
  if (options$newpage) grid.newpage()
  
  ## main
  gTitle(options$main, name="main", gp=options$gp_main)
  
  ## legend
  downViewport("main")
  grid.text(options$legend, 
    x=unit(1, "npc")-unit(1,"lines"),
    y=unit(-2, "lines"),
    just=c("right", "top"), gp=options$gp_legend)
  
  ### FIXME: a color and size key would be great!
  #gColorkey(c(0,1), options$col, label = "FIXME",
  # 	            name = "colorkey", gp = gpar())
  
  upViewport(1)
  
  # determine margins for labels
  # Note: rownames are LHSs!
  #topSpace <- max(stringWidth(rownames(x)))
  topSpace <- max(grobHeight(textGrob(rownames(x), rot = 90, 
    gp=options$gp_labels)))
  #, gp=options$gp_labels))
  #rightSpace <- max(stringWidth(colnames(x)))
  rightSpace <- max(grobWidth(textGrob(colnames(x), 
    gp=options$gp_labels)))
  
  # have 2 lines to the left and 2 lines from bottom 
  pushViewport(viewport(x=unit(2,"lines"), y=unit(2,"lines"), 
    just = c("left","bottom"),
    width = unit(1, "npc")-(rightSpace+unit(3+4,"lines")), 
    height = unit(1, "npc")-(topSpace+unit(4+4+3,"lines")),
    #xscale = c(1, nrow(x)), yscale = c(1, ncol(x)), 
    #xscale = c(.5, nrow(x)+.5), yscale = c(.5, ncol(x)+.5), 
    xscale = c(.5, nrow(x)+.5), yscale = c(0, ncol(x)+.5), 
    default.units = "native",
    #gp = options$gp_labels,
    name="grouped_matrix"))
  
  ## grid
  yLabPos <- unit(ncol(x), "native")
  xLabPos <- unit(nrow(x), "native") 
  
  for(i in 1:nrow(x))  grid.lines(x = c(i,i),  
    y=c(1, yLabPos),
    default.units = "native", gp=options$gp_lines)
  for(i in 1:ncol(x))  grid.lines(y = c(i,i),  
    x=c(1, xLabPos),
    default.units = "native", gp=options$gp_lines)
  
  ## symbols
  for (variable in 1:ncol(x)) {
    size <- x[, variable]
    shading <- y[, variable]
    shading <- options$col[map_int(shading, c(1, length(options$col)), from.range=c(0,1))]
    
    
    options$panel.function(ncol(x)-variable+1L, size, 
      shading, options$spacing)
  }
  
  
  ## labels
  yLabPos <- yLabPos + unit(1, "lines")
  xLabPos <- xLabPos + unit(1, "lines")
  grid.text(rownames(x), x = 1:nrow(x), y = yLabPos,
    rot = 90, just = "left", 
    default.units = "native",
    gp = options$gp_labels)
  
  
  grid.text(rev(colnames(x)), x = xLabPos, y = (1:ncol(x)), 
    just = "left", 
    default.units = "native",
    gp = options$gp_labels)

  if(suppressed_rhs > 0 && options$reverse) 
    grid.text(
      paste0("+ ", suppressed_rhs, " supressed"), 
      x = xLabPos, y = 0, 
      just = "left", 
      default.units = "native",
      gp = options$gp_labels)
  
   
  ## add lhs, rhs
  if(options$reverse) {
    grid.text("Items in LHS Group", 
      x = unit(1, "native")-unit(1,"lines"), y = yLabPos, 
      rot = 90, just = "left", 
      default.units = "native", 
      gp = options$gp_labs)
    grid.text("RHS", x = xLabPos,  
      y = unit(ncol(x), "native")+unit(1,"lines"), 
      just = "left", 
      default.units = "native", gp = options$gp_labs)
  }else{
    grid.text("RHS", 
      x = unit(1, "native")-unit(1,"lines"), y = yLabPos, 
      rot = 90, just = "left", 
      default.units = "native", 
      gp = options$gp_labs)
    grid.text("Items in LHS Group", x = xLabPos,  
      y = unit(ncol(x), "native")+unit(1,"lines"), 
      just = "left", 
      default.units = "native", gp = options$gp_labs)
  }
  
  
  upViewport(1)
}


panel.circles <- function (row, size, shading, spacing) 
{
  size[size == 0] <- NA
  #NAs are white
  shading[is.na(shading)] <- 1
  
  grid.circle(x = c(1:length(size)), y=row, r = size/2 * (1 - spacing), 
    default.units = "native", 
    gp = gpar(fill = shading, col = shading, alpha=.9))
}

panel.squares <- function (row, size, shading, spacing) 
{
  size[size == 0] <- NA
  shading[is.na(shading)] <- 1
  grid.rect(x = c(1:length(size)), y=row, width = size * (1 - spacing), 
    height = size * (1 - spacing), 
    default.units = "native", 
    gp = gpar(fill = shading, col = shading, alpha=.9))
}





