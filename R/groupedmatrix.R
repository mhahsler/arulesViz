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
    main =paste("Grouped matrix for", length(rules), "rules"),
    k = 20,
    aggr.fun=median, 
    ## fix lift so serveral plots are comparable (NA: take max)
    max.shading=NA,
    interactive = FALSE,
    col = heat_hcl(100),
    #col = hcl(c=0, l=seq(10,80, length.out=100)),
    newpage=TRUE
  ))
  
  
  x <- grouped_matrix_int(rules, measure, shading,
    k=control$k, 
    aggr.fun=control$aggr.fun, 
    max.shading=control$max.shading,
    col=control$col
  )
  
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
    y = I(rep(unit(-3, "lines"), 4)),
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
      plot(x, col = control$col)
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
grouped_matrix_int <- function(rules, measure, shading,
  k=15, aggr.fun=median, max.shading=NA, 
  col=hcl(c=0, l=seq(10,80, length.out=100))) {
  
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
    cl=cl, km= km, max.shading=max.shading, 
    aggr.fun=aggr.fun, 
    order=order, k=k, sAggr=sAggr, mAggr=mAggr)
  class(ret) <- "grouped_matrix"
  
  ## call plotting work horse
  plot(ret, col=col)
  
  ret
}

## display grouped_matrix
plot.grouped_matrix <- function(x, col = hcl(c=0, l=seq(10,80, length.out=100)), ...) {
  ## circle size
  sn <- x$mAggr
  ## shading
  ln <- x$sAggr
  
  ## get most important item in the lhs
  f <- lapply(split(x$rules, x$cl), FUN = function(r) itemFrequency(lhs(r)))
  most_imp_item <- lapply(f, FUN = 
      function(x) {
        items <- sum(x>0)
        if(items==0) { "" }
        else if(items>1){
          paste(names(which.max(x)), ", +",sum(x>0)-1, " items", sep="")
        }else{
          names(which.max(x))   
        }
      })
  
  grouped_matrix_plot_int(
    x = map(sn, c(0.2,1)), 
    y = map(ln, range = c(1,.2), 
      from.range = c(min(x$sAggr, na.rm=TRUE), x$max.shading)),
    order = x$order,
    options = list(
      panel = panel.circles, 
      spacing = -1, 
      reverse=TRUE,
      ylab=paste(#1:max(x$cl), "-",
        paste('{',most_imp_item, '}', " - ",
          table(x$cl), " rules",
          sep='')),
      main = paste("Grouped matrix for", length(x$rules), "rules"),
      legend = paste("size:",x$measure, "\ncolor:",x$shading),
      col = col
    )
  )
}

## inspect rules inside an grouped_matrix
inspect.grouped_matrix <- function(x, cluster, measure="lift") {
  inspect(sort(x$rules[x$cl==cluster], by=measure))
}


## workhorse for plotting
## based on bertinplot in package seriation
grouped_matrix_plot_int <- function (x, y, order = NULL, options = NULL) {
  if (!is.matrix(x)) 
    stop("Argument 'x' must be a matrix.")
  
  options <- .get_parameters(options, list(
    panel.function = panel.circles, 
    reverse = FALSE, 
    xlab = NULL, 
    ylab = NULL, 
    frame = FALSE, 
    spacing = 0.2, 
    gp_labels = gpar(cex=.8), 
    gp_panels = gpar(), 
    newpage = TRUE,
    main = "Grouped matrix",
    col = hcl(c=0, l=seq(10,80, length.out=100)),
    legend = ""
  ))
  
  if (!is.null(options$xlab)) rownames(x) <- options$xlab
  if (!is.null(options$ylab)) colnames(x) <- options$ylab
  
  if (!is.null(order)) {
    x <- permute(x, order)
    y <- permute(y, order)
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
  gTitle(options$main, name="main")
  
  ## legend
  downViewport("main")
  grid.text(options$legend, 
    x=unit(1, "npc")-unit(1,"lines"),
    y=unit(-2, "lines"),
    just=c("right", "top"), gp=options$gp_labels)
  
  ### FIXME: a color and size key would be great!
  #gColorkey(c(0,1), options$col, label = "FIXME",
  # 	            name = "colorkey", gp = gpar())
  
  upViewport(1)
  
  ## determine margins
  topSpace <- max(stringWidth(rownames(x)))
  rightSpace <- max(stringWidth(colnames(x)))
  
  pushViewport(viewport(x=unit(2,"lines"), y=unit(4,"lines"),
    just = c("left","bottom"),
    width = unit(1, "npc")-rightSpace-unit(3,"lines"), 
    height = unit(1, "npc")-topSpace-unit(4+3,"lines"),
    #xscale = c(1, nrow(x)), yscale = c(1, ncol(x)), 
    xscale = c(.5, nrow(x)+.5), yscale = c(.5, ncol(x)+.5), 
    default.units = "native", gp=options$gp_labels,
    name="grouped_matrix"))
  
  ## grid
  yLabPos <- unit(ncol(x), "native")
  xLabPos <- unit(nrow(x), "native") 
  
  gp_lines <- gpar(col="gray", lty=3)
  for(i in 1:nrow(x))  grid.lines(x = c(i,i),  
    y=c(1, yLabPos),
    default.units = "native", gp=gp_lines)
  for(i in 1:ncol(x))  grid.lines(y = c(i,i),  
    x=c(1, xLabPos),
    default.units = "native", gp=gp_lines)
  
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
    default.units = "native")
  # gpar is already set in viewport
  # gp = options$gp_labels)
  
  
  grid.text(rev(colnames(x)), x = xLabPos, y = (1:ncol(x)), 
    just = "left", 
    default.units = "native")
  # gpar is already set in viewport
  # gp = options$gp_labels)
  
  ## add lhs, rhs
  gp <- gpar(fontface = "bold", cex = 1.2)
  grid.text("LHS", 
    x = unit(1, "native")-unit(1,"lines"), y = yLabPos, 
    rot = 90, just = "left", 
    default.units = "native", 
    gp = gp)
  grid.text("RHS", x = xLabPos,  
    y = unit(ncol(x), "native")+unit(1,"lines"), 
    just = "left", 
    default.units = "native", gp = gp)
  
  
  
  upViewport(1)
}


panel.circles <- function (row, size, shading, spacing) 
{
  size[size == 0] <- NA
  #NAs are white
  shading[is.na(shading)] <- 1
  
  grid.circle(x = c(1:length(size)), y=row, r = size/2 * (1 - spacing), 
    default.units = "native", 
    gp = gpar(fill = shading, alpha=.9))
}

panel.squares <- function (row, size, shading, spacing) 
{
  size[size == 0] <- NA
  shading[is.na(shading)] <- 1
  grid.rect(x = c(1:length(size)), y=row, width = size * (1 - spacing), 
    height = size * (1 - spacing), 
    default.units = "native", 
    gp = gpar(fill = shading, alpha=.9))
}





