#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2021 Michael Hahsler
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

## create matrix with grouped LHS (columns) using measure
rules2groupedMatrix <- function(rules, measure = "lift", measure2 = "support", 
  k = 10,  aggr.fun = mean, lhs_label_items = 2) {
  
  m <- rules2matrix(rules, measure, reorder = "none")
  m_enc <- attr(m, "encoding")  ### encoding contains the rule index for each cell in matrix m
  m2 <- rules2matrix(rules, measure2, reorder = "none")
  
  ## check k
  k <- min(ncol(m), k)
  
  ## FIXME: this handling of NA for clustering is not great!
  m_clust <- t(m)
  naVal <- if(measure == "lift") 1 else 0
  m_clust[is.na(m_clust)] <- naVal
  
  ## are there enough non-identical points for k-means?
  ## if not then we group the identical ones using hclust
  if(sum(!duplicated(m_clust)) > k) 
      clustering_lhs <- stats::kmeans(m_clust, k, iter.max = 100, nstart = 20)$cluster
  else 
      clustering_lhs <- stats::cutree(stats::hclust(stats::dist(m_clust)), h = 0)
  
  # aggregate the data
  m_aggr <- .aggr(m, clustering_lhs, aggr.fun)
  m2_aggr <- .aggr(m2, clustering_lhs, aggr.fun)
  
  ### calculate cluster membership for rules
  clustering_rules <- vector(mode = "integer", length = ncol(m_enc))
  for(i in seq_len(ncol(m_enc))) clustering_rules[m_enc[, i]] <- clustering_lhs[i]
  
  ## get most important item for each lhs cluster
  f <- lapply(split(rules, clustering_rules), FUN = function(r) itemFrequency(lhs(r), 
    type = "absolute"))
  ## divide by sum to find most important item...
  f <- lapply(f, "/", itemFrequency(lhs(rules), type = "absolute")+1L)
  
  most_imp_item <- lapply(f, FUN = 
      function(x) {
        items <- sum(x > 0)
        if(items == 0) { "" }
        else if(lhs_label_items < 1){ 
          paste(items, "items") 
        } else if(items > lhs_label_items){
          paste(paste(names(x[head(order(x, decreasing = TRUE), n = lhs_label_items)]), 
            collapse = ", "), ", +", items - lhs_label_items, " items", sep = "")
        }else{
          paste(names(x[head(order(x, decreasing = TRUE), n = items)]), 
            collapse = ", ")
        }
      })
  
  lhs_cluster_labels <- paste(
    paste(format(table(clustering_rules)), " rules: ", '{', most_imp_item, '}', sep = ''))
  
  colnames(m_aggr) <- lhs_cluster_labels
  colnames(m2_aggr) <- lhs_cluster_labels
  
  # order decreasing starting in the top-left corner
  o <- seriation::ser_permutation(
    order(apply(m_aggr, MARGIN = 1, FUN = aggr.fun, na.rm = TRUE), decreasing = TRUE),
    order(apply(m_aggr, MARGIN = 2, FUN = aggr.fun, na.rm = TRUE), decreasing = TRUE)
  )
  
  m_aggr <- seriation::permute(m_aggr, o)
  m2_aggr <- seriation::permute(m2_aggr, o)
  clustering_rules <- seriation::get_rank(o[[2]])[clustering_rules]
  
  list(m = m_aggr, m2 = m2_aggr, clustering_rules = clustering_rules)
}  

## helper
.aggr <- function(m, cl, aggr.fun = median) {
  ma <- matrix(nrow=nrow(m), ncol=0)
  for(i in 1:max(cl)) {
    ma <- cbind(ma, apply(m[, cl==i, drop=FALSE], 
      MARGIN=1, aggr.fun , na.rm=TRUE))
  }
  ma[!is.finite(ma)] <- NA
  ma
}



grouped_matrix_plot <- function(rules, measure, shading, control = NULL, ...){
  
  ## measure controls circle size
  ## shading controls color
 
  engines <- c("default", "grid", "interactive", "ggplot2", "htmlwidget")
  if(control$engine == "help") {
    message("Available engines for this plotting method are:\n", paste0(engines, collapse = ", "))
    return(invisible(engines))  
  }
  
  m <- pmatch(control$engine, engines, nomatch = 0)
  if(m == 0) stop("Unknown engine: ", sQuote(control$engine), 
    " Valid engines: ", paste(sQuote(engines), collapse = ", "))
  control$engine <- engines[m] 
  
  if(pmatch(control$engine, c("grid", "interactive"), nomatch = 0) >0) { 
    return(grouped_matrix_grid(rules, measure, shading, control, ...)) 
  }
  
  ### default is ggplot2
  return(grouped_matrix_ggplot2(rules, measure, shading, control, ...)) 
}
  
grouped_matrix_grid <- function(rules, measure, shading, control = NULL, ...){
  control <- c(control, list(...))
  control <- .get_parameters(control, list(
    main = paste("Grouped Matrix for", length(rules), "Rules"),
    k = 20,
    rhs_max = 10,
    lhs_label_items = 2,
    aggr.fun = mean, 
    ## fix lift so several plots are comparable (NA: take max)
    col = default_colors(100),
    legend = paste("Size:", measure, "\nColor:", shading),
    spacing = -1, 
    panel.function = panel.circles, 
    gp_main   = gpar(cex = 1.2, fontface = "bold"),
    gp_labels = gpar(cex = .8), 
    gp_labs   = gpar(cex = 1.2, fontface = "bold"),
    gp_lines  = gpar(col = "gray", lty = 3),
    newpage = TRUE,
    max.shading = NA,
    engine = "default"
  ))
  
  x <- grouped_matrix_int(rules, measure, shading, 
    k = control$k, aggr.fun = control$aggr.fun, lhs_label_items = control$lhs_label_items,
    max.shading = control$max.shading) 
  plot(x, control = control)
  
  if(control$engine !="interactive") return(invisible(x))
  
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
    if(b == "end") {
      cat("Leaving interactive mode.\n")
      return(rules)
    }
    
    if(b == "zoom out") {
      cat("Going up.\n")
      return("zoom out")
    }
    
    
    select <- convertLoc(selection(gI)$loc, "native", valueOnly = TRUE)$x
    if(is.null(select)) {
      cat("Select a LHS first!\n")
      gI <- resetButtons(gI)
      next
    }
    
    select <- round(select) 
    
    if(select > 0 && select <= control$k) {
      rulesSelected <- rules[x$cl == select]
    }else{
      cat("Illegal selection! Choose a LHS.\n")
      next
    }
    
    if(b == "zoom in") {
      if(length(unique(lhs(rulesSelected)))<2) {
        cat("Can't zoom in any further. Use inspect!\n")
        gI <- changeButton(gI, "zoom in", FALSE)
        next
      }
      
      cat("Zooming in. This might take a while\n")
      
      ret <- grouped_matrix_plot(rulesSelected, measure, 
        shading, control)
      
      if(!identical(ret, "zoom out")) return(ret)
      
      ## we come back up so replot
      plot(x, control=control)
      seekViewport("grouped_matrix")
      gI <- resetButtons(gI)
    }
    
    if(b == "inspect") {
      cat("Selected rules:\n")
      ## TODO: click on bubble
      #selectRHS <- round(as.numeric(convertY(location$Y, "native")))
      inspect(sort(rulesSelected, by = "lift"))
      gI <- changeButton(gI, "inspect", FALSE)
    }
    
    ## nothing else to do
    next
  }
}

grouped_matrix_int <- function(rules, measure = "support", shading = "lift",
  k = 20, aggr.fun = mean, lhs_label_items = 2, max.shading = NA) {
  
  gm <- rules2groupedMatrix(rules, shading, measure, k = k, 
    aggr.fun = aggr.fun, lhs_label_items = lhs_label_items)
 
  if(is.na(max.shading)) max.shading <- max(gm$m, na.rm=TRUE)
  
  structure(list(
    rules = rules, 
    measure = measure, 
    shading = shading, 
    lhs_label_items = lhs_label_items, 
    max.shading = max.shading, 
    aggr.fun = aggr.fun, 
    k = k, 
    sAggr = gm$m, 
    mAggr = gm$m2,
    clustering = gm$clustering_rules
  ),
    class = "grouped_matrix")
}

## inspect rules inside an grouped_matrix
inspect.grouped_matrix <- function(x, cluster, measure = "lift") {
  inspect(sort(x$rules[x$clustering == cluster], by = measure))
}


## display grouped_matrix
plot.grouped_matrix <- function(x, ...) {
  control <- list(...)$control
  
  # ms ... size matrix, mc ... color shading matrix
  ms <- map(x$mAggr, c(0.2,1)) 
  mc <- map(x$sAggr, range = c(1,.2))
  from.range <- c(min(x$sAggr, na.rm = TRUE), x$max.shading)
  
  ### only show the top RHS
  suppressed_rhs <- 0
  if(!is.null(control$rhs_max) && 
      control$rhs_max > 0 && 
      nrow(ms) > control$rhs_max) {
    suppressed_rhs <- nrow(ms) - control$rhs_max
    ms <- ms[1:control$rhs_max, , drop = FALSE]
    mc <- mc[1:control$rhs_max, , drop = FALSE]
  }
  
  rows <- nrow(ms)
  cols <- ncol(ms)
  
  if (control$newpage) grid.newpage()
  
  ## main
  gTitle(control$main, name = "main", gp = control$gp_main)
  
  ## legend
  downViewport("main")
  grid.text(control$legend, 
    x = unit( 1, "npc") - unit(1, "lines"),
    y = unit(-2, "lines"),
    just = c("right", "top"), gp = control$gp_legend)
  
  ### FIXME: a color and size key would be great!
  #gColorkey(c(0,1), options$col, label = "FIXME",
  # 	            name = "colorkey", gp = gpar())
  
  upViewport(1)
  
  # determine margins for labels
  # Note: rownames are LHSs!
  #topSpace <- max(stringWidth(rownames(x)))
  topSpace <- max(grobHeight(textGrob(colnames(ms), rot = 90, 
    gp = control$gp_labels)))
  #, gp=options$gp_labels))
  #rightSpace <- max(stringWidth(colnames(x)))
  rightSpace <- max(grobWidth(textGrob(rownames(ms), 
    gp = control$gp_labels)))
  
  # have 2 lines to the left and 2 lines from bottom 
  pushViewport(viewport(x = unit(2, "lines"), y = unit(2, "lines"), 
    just = c("left", "bottom"),
    width = unit(1, "npc") - (rightSpace + unit(3 + 4, "lines")), 
    height = unit(1, "npc") - (topSpace + unit(4 + 4 + 3, "lines")),
    xscale = c(.5, cols + .5), 
    yscale = c(0, rows + .5), 
    default.units = "native",
    #gp = options$gp_labels,
    name = "grouped_matrix"))
  
  ## grid
  yLabPos <- unit(rows, "native")
  xLabPos <- unit(cols, "native") 
  
  for(i in 1:cols)  grid.lines(x = c(i, i),  
    y = c(1, yLabPos),
    default.units = "native", gp = control$gp_lines)
  for(i in 1:rows)  grid.lines(y = c(i, i),  
    x = c(1, xLabPos),
    default.units = "native", gp = control$gp_lines)
  
  ## place symbols
  for (variable in 1:rows) {
    size <- ms[variable, ]
    shading <- mc[variable, ]
    shading <- control$col[map_int(shading, c(1, length(control$col)), from.range = c(0, 1))]
    control$panel.function(rows - variable + 1L, size, shading, control$spacing)
  }
  
  ## labels
  yLabPos <- yLabPos + unit(1, "lines")
  xLabPos <- xLabPos + unit(1, "lines")
  grid.text(colnames(ms), 
    x = 1:cols, 
    y = yLabPos,
    rot = 90, 
    just = "left", 
    default.units = "native",
    gp = control$gp_labels)
  
  grid.text(rev(rownames(ms)), 
    x = xLabPos, 
    y = (1:rows), 
    just = "left", 
    default.units = "native",
    gp = control$gp_labels)

  if(suppressed_rhs > 0) 
    grid.text(
      paste0("+ ", suppressed_rhs, " supressed"), 
      x = xLabPos, 
      y = 0, 
      just = "left", 
      default.units = "native",
      gp = control$gp_labels)
  
  grid.text("Items in LHS Group", 
    x = unit(1, "native")-unit(1,"lines"), y = yLabPos, 
    rot = 90, just = "left", 
    default.units = "native", 
    gp = control$gp_labs)
  grid.text("RHS", x = xLabPos,  
    y = unit(rows, "native")+unit(1,"lines"), 
    just = "left", 
    default.units = "native", gp = control$gp_labs)
  
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