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


scatterplot_arules <- function(rules, measure = c("support","confidence"), 
  shading = "lift", control = NULL, ...){
  
  control <- c(control, list(...))  
  
  control <- .get_parameters(control, list(
    main =paste("Scatter plot for", length(rules), class(rules)),
    interactive = FALSE,
    pch = 19,
    cex = .5,
    xlim = NULL,
    ylim = NULL,
    zlim = NULL,
    alpha = NULL,
    col = default_colors(100),
    #col = hcl(c=0, l=seq(10,80, length.out=100)),
    #col = heat_hcl(100),
    #gray_range = c(.1,.8),
    newpage = TRUE,
    jitter = NA
  ))
  
  
  ## set zlim depending on measure...
  ## add order
  quality(rules) <- cbind(quality(rules), order=size(rules))
  
  if(!is.na(shading)) {
    i <- pmatch(shading, colnames(quality(rules)))
    if(is.na(i)) stop("Unknown quality measure for shading.")
    shading <- colnames(quality(rules))[i]
    
    ## fix zlim for some known measures!
    if(is.null(control$zlim)) {
      if(shading == "lift") 
        control$zlim <- c(min(1, min(quality(rules)["lift"])), 
          max(quality(rules)["lift"]))
    }
    
  }
  
  ## call workhorse
  scatterplot_int(rules, measure, shading, control, ...)
  
  if(!control$interactive) return(invisible())
  
  ## interactive mode
  cat("Interactive mode.\nSelect a region with two clicks!\n")
  
  ## go to scatterplot viewport
  downViewport("scatterplot")
  
  ## add buttons
  gI <- gInteraction(data.frame(
    row.names = c("inspect", "filter","zoom in", "zoom out", "end"),
    active = rep(FALSE, 5),
    x = c(0.1, 0.3, 0.5, 0.7, 0.9),
    y = I(rep(unit(-4.5, "lines"), 5)),
    w = I(rep(unit(3.5, "lines"), 5)),
    h = I(rep(unit(1, "lines"), 5))
  )
  )
  
  drawButtons(gI)
  
  q <- quality(rules)[, c(measure[1], measure[2])]
  sel_r <- rules
  
  while(TRUE){
    gI <- gGetEvent(gI)
    
    b <- lastButton(gI)
    if(is.null(b)) next
    
    ## actions
    if(b=="end") {
      ## fixme: is a pop missing?
      cat("Leaving interactive mode (returning selection).\n")
      return(sel_r)
    }
    
    if(b=="filter") {
      if(is.na(shading) || shading=="order") {
        cat("No filtering for order/no shading!\n")
        gI <- changeButton(gI,"filter", FALSE)
        next
      }
      
      cat("Select minimum", shading,"in colorkey.\n")
      seekViewport("colorkey")
      location <- grid.locator()
      if(insidePlot(location)) {
        colSel <- gPointSelection(location)
        sel_r <- rules[quality(rules)[,shading]>=
            convertLoc(colSel$loc, "native", valueOnly=TRUE)$y]
        
        if(length(sel_r)>1) {
          
          if(is.null(control$xlim)) control$xlim <- range(q[,1])
          if(is.null(control$ylim)) control$ylim <- range(q[,2])
          
          ret <- scatterplot_arules(sel_r, measure, 
            shading, control)
          if(!identical(ret, "zoom out")) return(ret)
          
          ## replot and reset
          scatterplot_int(rules, measure, shading, control, ...)
          downViewport("scatterplot")
          gI <- resetButtons(gI)
          #drawButton(gI)
        }else{
          cat("Not enough rules pass the filter!\n")
        }
      }
      seekViewport("scatterplot")
      gI <- changeButton(gI, "filter", FALSE)
      
    }
    
    if(b=="zoom out") {
      ## fixme: is a pop missing?
      cat("Going up.\n")
      return("zoom out")
    }
    
    ## zoom in if inside selection box
    if(b=="zoom in") {
      sel <- selection(gI)
      if(is.null(sel) || !is(sel, "gBoxSelection")) {
        ## no box selected!
        cat("Select a region first!\n")
        gI <- changeButton(gI, "zoom in", FALSE)
        next
      }
      
      sel_r <- rules[filterSelection(sel, q)]
      
      if(length(sel_r)<2) {
        cat("Select more rules!\n")
        next
      }
      
      ## xlim, ylim for zooming makes no sense
      control$xlim <- NULL
      control$ylim <- NULL
      
      ret <- scatterplot_arules(sel_r, measure, 
        shading, control)
      if(!identical(ret, "zoom out")) return(ret)
      
      ## replot and reset
      scatterplot_int(rules, measure, shading, control, ...)
      downViewport("scatterplot")
      
      gI <- resetButtons(gI)
      #drawButton(gI)
    }
    
    if(b=="inspect") {
      gI <- changeButton(gI, "inspect", FALSE)
      sel <- selection(gI)
      
      if(is.null(sel)) {
        cat("Nothing selected!\n")
        next
      }
      
      sel_r <- rules[filterSelection(sel, q)]
      
      if(length(sel_r) > 0) {
        cat("\nNumber of rules selected:", length(sel_r),"\n")
        inspect(sort(sel_r, by = shading))
        cat("\n")
      } else cat("No rules selected!\n")
      
    }
    
    ## unknown button
    next
  }
}


scatterplot_int <- function(rules, measure, shading, control, ...){
  
  ## reverse colors
  colors <- rev(control$col)
 
   
  q <- quality(rules)[, na.omit(c(measure, shading))]
  
  ## handle Inf
  for(i in 1:ncol(q)) {
    infin <- is.infinite(q[[i]])
    if(any(infin)) {
      replinfin <- signif(2 * max(q[[i]][!infin], na.rm = TRUE), 3)
      warning(colnames(q)[i], " contains infinite values! Replaced by twice the max (", replinfin, ")!")
      q[[i]][infin] <- replinfin
    }
 }
  
  if(control$newpage) grid.newpage()
  
  if(control$interactive) addspace <- 2.5
  else addspace <- 0
  
  ## main
  gTitle(control$main)
  
  ## colorkey
  if(!is.na(shading)) {
    pushViewport(viewport(x=unit(1, "npc")-unit(3+2, "lines"), 
      y=unit(4+addspace, "lines"),
      height=unit(1,"npc")-unit(4+4+addspace, "lines"),
      width=unit(2, "lines"),
      just = c("left", "bottom")))
    
    
    ## shading range    
    if(is.null(control$zlim)) range_shading <- range(q[[shading]])
    else range_shading <- control$zlim
    
    if(shading=="order") {
      max_size <- max(q$order)
      min_size <- min(q$order)
      steps <- (max_size-min_size)+1
      ypos <- rev((1:steps -.5)/steps)
      #col <- gray(map(min_size:max_size, control$gray_range))
      col <- colors[map_int(min_size:max_size, c(1,length(colors)))]
      grid.points(x=rep(0,steps), 
        y=ypos, pch=control$pch,
        gp=gpar(col=rev(col), fill=rev(col), alpha=control$alpha, cex = control$cex),
        size=unit(.5,"npc"))
      grid.text(paste("order", max_size:min_size, sep=" "), 
        x=rep(1,steps), y=ypos)
      
    } else {
      
      if(diff(range_shading) != 0) {
        
        gColorkey(range_shading,
          colors,
          #gray(1-map(1:20, control$gray_range)),
          name="colorkey", label=shading)
      }else{
        grid.text(paste(shading, "=", 
          round(range_shading[1],3)), .5, 
          unit(-1,"lines"))
      }
      
    }
    
    ## reduce overplotting
    o <- order(q[[shading]])
    q <- q[o,]
    
    upViewport(1)
  }
  
  ## scatterplot
  pushViewport(viewport(x=unit(4, "lines"), 
    y=unit(4+addspace, "lines"),
    height=unit(1,"npc")-unit(4+4+addspace, "lines"),
    width=unit(1,"npc")-unit(4+2+3+2, "lines"),
    just = c("left", "bottom")))
  
  x <- q[, c(measure[1], measure[2])]
 
  control$jitter <- control$jitter[1]
  if(is.na(control$jitter) && any(duplicated(x))) {
    #warning("To reduce overplotting, jitter is set to 1!")
    control$jitter <- .1
    }
   
  if(!is.na(control$jitter) && control$jitter>0) {
    x[,1] <- jitter(x[,1], factor=control$jitter, amount = 0)
    x[,2] <- jitter(x[,2], factor=control$jitter, amount = 0)
  }
  
  
  ## get colors for shading
  if(!is.na(shading)) {
    #col <- 1-map(q[[shading]], 
    #	control$gray_range, from.range=range_shading)
    col <- colors[map_int(q[[shading]], 
      c(1,length(colors)), from.range=range_shading)]
    ## not in range!
    #x[is.na(col),] <- c(NA,NA) 
    #col[is.na(col)] <- 1
    #col <- gray(col)
    
  }else col <- 1
  
  gScatterplot(x, 
    xlim=control$xlim, ylim=control$ylim, 
    xlab=measure[1], ylab=measure[2],
    col=col, cex=control$cex, alpha=control$alpha, pch=control$pch,
    name="scatterplot", new=FALSE)
  
  
  upViewport(1)
}

