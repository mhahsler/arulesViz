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


## simple image plot using grid

gTitle <- function(main, height=unit(4, "lines"), 
  gp=gpar(fontface="bold", cex=1.2), name=NULL) {
  pushViewport(viewport(y=1, height=height,
    just = c("top"), name=name))
  grid.text(main,
    gp=gp)
  upViewport(1)
}

gImage <- function(m, 
  xScale = NULL, yScale = NULL, xlab=NULL, ylab=NULL, name = NULL, axes = TRUE, gp = gpar(), ...) {
  
  df <- which(!is.na(m), arr.ind=TRUE)
  y <- df[,1] -1L
  x <- df[,2] -1L
  z <- as.vector(m[!is.na(m)])
  
  
  if(is.null(xScale)) {
    width <- 1
    xScale <- c(1,ncol(m)+1L)-.5
    x <- x+.5
  } else {
    width <- diff(xScale)/ncol(m)
    xS <- c(xScale[1] + width/2, xScale[2] - width/2) - width/2
    x <- map(x, xS)
  }
  
  if(is.null(yScale)) {
    height <- 1
    yScale <- c(0,nrow(m))+.5
    y <- y+.5
  } else {
    height <- diff(yScale)/nrow(m)
    yS <- c(yScale[1] + height/2, yScale[2] - height/2) - height/2
    y <- map(y, yS)
  }
  
  ## create viewport
  vp <- viewport(
    xscale = xScale, yscale = yScale,
    default.units = "native", name = name)
  pushViewport(vp)
  
  ## draw squares
  grid.rect(x = x, y = y, width, height,
    gp = gpar(fill = z, col=0), just=c("left","bottom"),
    default.units = "native")
  
  ## border and axes
  gp_border       <- gp
  gp_border$fill  <- "transparent"
  grid.rect(x = 0, y = 0,
    width = 1, height = 1, just=c("left","bottom"),
    default.units = "npc", gp = gp_border)
  
  if(is.logical(axes) && axes) {
    grid.xaxis(gp = gp)
    grid.yaxis(gp = gp)
  }
    
  if(is.character(axes) && axes == "integer") {
    grid.xaxis(gp = gp, at = 1:ncol(m))
    grid.yaxis(gp = gp, at = 1:nrow(m))
  }
    
  ## labels
  if(!is.null(xlab)) grid.text(xlab, 0.5, unit(-3, "lines"))
  if(!is.null(ylab))  grid.text(ylab, unit(-3, "lines"), 0.5, rot=90)
  
  upViewport(1)
}


gScatterplot <- function(x, y=NULL, xlim = NULL, ylim = NULL,
  xlab = "x", ylab="y", 
  col="black", cex=1, pch=1, alpha = NULL,
  new = TRUE, main = "Scatterplot", 
  name = NULL, 
  gp = gpar()) {
  
  if(is(x, "matrix") || is(x, "data.frame")) {
    y <- x[,2]
    x <- x[,1]
  }
  
  ## scale and filter points
  if(is.null(xlim)) xlim <- range(x, na.rm=TRUE)
  else x[x<xlim[1] | x>xlim[2]] <- NA
  if(is.null(ylim)) ylim <- range(y, na.rm=TRUE)
  else y[y<ylim[1] | y>ylim[2]] <- NA
  
  ## handle a single vaule for lim
  if(diff(xlim)==0) xlim <- xlim*c(0.5, 1.5)
  if(diff(ylim)==0) ylim <- ylim*c(0.5, 1.5)
  
  ## make plotting region 5% larger
  width <- diff(xlim)
  height <- diff(ylim)
  xlim[1] <- xlim[1]-width*0.025
  xlim[2] <- xlim[2]+width*0.025
  ylim[1] <- ylim[1]-height*0.025
  ylim[2] <- ylim[2]+height*0.025
  
  ## new plot
  if(new) {
    grid.newpage()
    pushViewport(plotViewport())
    grid.text(main, .5, unit(1, "npc")+unit(2, "lines"),
      gp=gpar(fontface="bold", cex=1.2))
  }
  
  pushViewport(viewport(clip=FALSE, xscale=xlim, yscale=ylim, 
    name= name))
  ## axes
  grid.xaxis()
  grid.yaxis()
  grid.text(xlab, .5, unit(-3, "lines"))
  grid.text(ylab, unit(-3.5, "lines"), .5, rot=90)
  
  ## points
  grid.points(x, y, pch=pch, 
    gp=gpar(col=col, fill=col, alpha=alpha, cex=cex))
  
  ## box
  grid.rect(x = 0, y = 0, width = 1, height = 1,
    just = c("left", "bottom"), default.units = "npc",
    gp = gpar(fill = "transparent"))
  
  upViewport(1)
  if(new) upViewport(1)
}

gColorkey <- function(range, col, label = NULL,
  name = "colorkey", gp = gpar()) {
  
  n <- length(col) 
  height <- diff(range)/n
  ys <- seq(range[1] + height/2, range[2] - height/2, length.out = n)
  
  vp <- viewport(
    xscale = c(0,1), yscale = c(range[1]+height/2, range[2]+height/2), 
    default.units = "native", name = name)
  pushViewport(vp)
  
  ## col
  gp_col      <- gp
  gp_col$col  <- 0
  gp_col$fill <- col
  grid.rect(x = 0, y = ys, width = 1, height = height,
    just = c("left", "bottom"), default.units = "native",
    gp = gp_col)
  
  
  ## box
  gp_border       <- gp
  gp_border$fill  <- "transparent"
  grid.rect(x = 0, y = 0, width = 1, height = 1,
    just = c("left", "bottom"), default.units = "npc",
    gp = gp_border)
  
  grid.yaxis(gp = gp, main=FALSE)
  
  ## label
  if(!is.null(label)) grid.text(label,0.5,unit(-1, "lines"))
  
  upViewport(1)
}

gParacoords <- function(m, discreteNames=NULL,
  col=NULL, lwd=NULL, arrowPos=NULL, 
  xlab=NULL, name=NULL, gp=gpar(), gp_lines=gpar()) {
  
  if(is.null(col)) col <- 1
  if(length(col)==1) col <- rep(col, nrow(m))
  if(is.null(lwd)) lwd <- 1
  if(length(lwd)==1) lwd <- rep(lwd, nrow(m))
  if(is.null(arrowPos)) arrowPos <- 0
  if(length(arrowPos)==1) arrowPos <- rep(arrowPos, nrow(m))
  
  arrow_type <- arrow(angle = 15)
  
  if(is.null(discreteNames)) {
    #leftSpace <- unit(3, "lines")
    yscale <- range(as.numeric(m), na.rm=TRUE)
  }else{ 
    #leftSpace <- max(stringWidth(discreteNames))
    yscale <- c(.5, length(discreteNames)+.5)
  }
  
  pushViewport(viewport(xscale = c(.5, ncol(m)+.5), 
    yscale = yscale,
    default.units = "native", gp=gp,
    name=name))
  
  ## axes
  grid.xaxis(at=1:ncol(m), label=colnames(m))
  if(is.null(discreteNames)) grid.yaxis()
  else grid.yaxis(at= 1:length(discreteNames), label=discreteNames)
  if(!is.null(xlab)) grid.text(xlab, .5, unit(-3, "lines"))
  
  ## box
  grid.rect(x = 0, y = 0, width = 1, height = 1,
    just = c("left", "bottom"), default.units = "npc",
    gp = gpar(fill = "transparent"))
  
  ## grid
  gp_grid <- gpar(col="gray", lty=3)
  for(i in 1:ncol(m))  grid.lines(x = c(i,i), y=unit(c(0,1), "npc"),
    default.units = "native", gp=gp_grid)
  
  ## draw lines
  for(i in 1:nrow(m)) {
    line <- m[i,]
    gp_lines$lwd <- lwd[i]
    gp_lines$col <- col[i]
    arr <- arrowPos[i]
    
    for(j in 1:length(line)) {
      grid.lines(y=line[j:(j+1L)], x=c(j, j+1L),
        default.units = "native", gp=gp_lines,
        arrow= if((j+1L)==arr) arrow_type else NULL)
    }
  }
  
  upViewport(1)
}





