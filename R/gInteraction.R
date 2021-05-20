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


## Simple Grid Interactions

## gInteraction class

gInteraction <- function(buttons=data.frame(), buttonCol=NULL) {
    if(is.null(buttonCol)) buttonCol <- c("lightblue","orange")
    
    #do some checks
    if(!is(buttons, "data.frame")) stop("gInteraction expects a data.frame to specify 'buttons'!")
    if(nrow(buttons)>0) {
	if(any(is.na(match(c("active","y","x","w","h"), 
					names(buttons))))) 
	stop("gInteraction: 'buttons' does not contain all required information!")

	for(f in c("x", "y", "w", "h")) 
	    if(!is(buttons[,f], "unit")) buttons[,f] <- I(unit(buttons[,f], "npc"))
    }

    ## last contains the last button pressed
    ## sel contains the current selection
    structure(
	    list(buttons=buttons, buttonCol=buttonCol, 
		    lastButton=NULL, sel=NULL),
	    class = "gInteraction"
	    )
}

print.gInteraction <- function(object) {
    writeLines(paste("Object of class 'gInteraction' with", 
		    nrow(object$buttons), "buttons."))

    print(t(object$buttons[, "active", drop=FALSE]))
}

## functions for buttons
drawButtons <- function(gI, which=NULL) {
    if(nrow(gI$buttons)<1) return(invisible()) 
    
    if(!is.null(which)) gI$buttons <- gI$buttons[which,] 
    for(i in 1:nrow(gI$buttons)) {
	b <- gI$buttons[i,]

	grid.rect(x=b$x, y=b$y, width=b$w, height=b$h, 
		default.units="npc", 
		gp=gpar(col=gI$buttonCol[b$active+1L],
			fill=gI$buttonCol[b$active+1L]))

	grid.text(rownames(b), b$x, y=b$y, 
		default.units="npc", 
		gp=gpar(col="black"))
    }
}

lastButton <- function(gI, activeOnly=TRUE) {
    if(activeOnly && !isActiveButton(gI, gI$lastButton)) return(NULL)
    else return(gI$lastButton)
}

selection <- function(gI) gI$sel

isActiveButton <- function(gI, which=NULL) {
    ## fixme: check if button exists
    if(nrow(gI$buttons)<1) return(logical(0)) 
    
    if(is.null(which)) which <- rownames(gI$buttons)
    structure(gI$buttons[which,]$active, names = which)
}

changeButton <- function(gI, which=NULL, to=NULL, redraw=TRUE) {
    ## fixme: check if button exists
    if(nrow(gI$buttons)<1) return(gI$buttons) 

    if(is.null(which)) which <- rownames(gI$buttons)
    if(is.null(to)) to <- !gI$buttons[which,]$active
    gI$buttons[which,]$active <- to
    if(redraw) drawButtons(gI, which)
    gI
}

resetButtons <- function(gI, redraw=TRUE) {
    gI <- changeButton(gI, to=FALSE, redraw=redraw)
    gI$lastButton <- NULL
    gI$sel <- NULL
    gI
}

whichButton <- function(gI, location) {
    if(nrow(gI$buttons)<1) return(NULL) 

    ## we need npc
    location$x <- convertX(location$x, "npc", valueOnly=TRUE)
    location$y <- convertY(location$y, "npc", valueOnly=TRUE)

    pressed <- NULL
    
    for(i in 1:nrow(gI$buttons)) {
	b <- gI$buttons[i,]
	
	if(location$x > convertX(b$x, "npc", valueOnly=TRUE)
		    -convertX(b$w, "npc", valueOnly=TRUE)/2 
		&& location$x < convertX(b$x, "npc", valueOnly=TRUE)
		+convertX(b$w, "npc", valueOnly=TRUE)/2 
		&& location$y > convertY(b$y, "npc", valueOnly=TRUE)
		-convertY(b$h, "npc", valueOnly=TRUE)/2
		&& location$y < convertY(b$y, "npc", valueOnly=TRUE)
		+convertY(b$h, "npc", valueOnly=TRUE)/2
		) {

	    pressed <- rownames(b)
	    gI <- changeButton(gI, pressed)
	    break
	}
    }
    
    gI$lastButton <- pressed

    gI
}

## Selection classes

gPointSelection <- function(loc, col="red") {
    loc <- convertLoc(loc, "native")
    name <- paste(c("sel", unlist(loc)), collapse="-")
    
    grid.points(x=loc$x, y=loc$y, pch=3, gp = gpar(col=col, cex=0.5),
	    name=name)
    
    structure(list(loc=loc, name=name), class = "gPointSelection")
}

gBoxSelection <- function(loc1, loc2, box=TRUE , col="red") {
    if(is(loc1, "gPointSelection")) loc1 <- loc1$loc
    if(is(loc2, "gPointSelection")) loc2 <- loc2$loc
    
    loc1 <- convertLoc(loc1, "native")
    loc2 <- convertLoc(loc2, "native")
    name <- paste(c("sel", unlist(loc1), unlist(loc2)), collapse ="-")

    locs <- rbind(unlist(loc1), unlist(loc2))
    x <- sort(locs[,1])
    y <- sort(locs[,2])

    ## draw box
    if(box) grid.rect(x=x[1], y=y[1], width=diff(x), height=diff(y),
	    default.units = "native", just=c("left", "bottom"),
	    gp=gpar(col=col, fill=col, alpha=0.1),
	    name=name)

    structure(list(loc=loc1, loc2=loc2, name=name), class = "gBoxSelection")
}

## S3 dispatch for filterSelection
filterSelection <- function(sel, ...) UseMethod("filterSelection")
foo.default <- function(sel, ...) stop("Unknown gSelection type!")

filterSelection.gBoxSelection <- function(sel, x) {
    locs <- rbind(unlist(convertLoc(sel$loc, "native", valueOnly=TRUE)),
	    unlist(convertLoc(sel$loc2, "native", valueOnly=TRUE)))


    x[,1] >= min(locs[,"x"]) & x[,1] <= max(locs[,"x"]) & 
    x[,2] >= min(locs[,"y"]) & x[,2] <= max(locs[,"y"])
}

filterSelection.gPointSelection <- function(sel, x, sensitivity=0.01) {
    loc1 <- sel$loc
    loc1$x <- loc1$x-unit(sensitivity, "npc")
    loc1$y <- loc1$y+unit(sensitivity, "npc")
    loc2 <- sel$loc
    loc2$x <- loc2$x+unit(sensitivity, "npc")
    loc2$y <- loc2$y-unit(sensitivity, "npc")

    filterSelection(gBoxSelection(loc1, loc2, box=FALSE), x)
}

clearSelection <- function(sel, redraw=TRUE) {
    ## make box invisible 
    ## maybe the is a way to use its own viewport for selections
    grid.edit(sel$name, gp = gpar(alpha=0), redraw=redraw)
}

## identifyer
gIdentify <- function(gI, checkPlotBoundaries=TRUE) {
	location <- grid.locator("npc")
	if(!is.null(gI$sel)) { 
	    clearSelection(gI$sel)
	    gI$sel <- NULL
	}
	
	if(checkPlotBoundaries && !insidePlot(location)) return(gI)
	
	gI$sel <- gPointSelection(location)
	gI
}

## simple event loop
gGetEvent <- function(gI, box=TRUE, checkPlotBoundaries=TRUE) {
    
    while(TRUE){
	location <- grid.locator("npc")
	gI <- whichButton(gI, location)
	b <- gI$lastButton

	if(!is.null(b)) {
	    ## buttons
	    if(isActiveButton(gI, b)) return(gI)
	
	}else{
	    ## selection
	    if(checkPlotBoundaries && !insidePlot(location)) next

	    if(is(gI$sel, "gPointSelection") && !box) {
		clearSelection(gI$sel)
		gI$sel <- NULL
	    }
	    
	    if(is(gI$sel, "gBoxSelection")) {
		clearSelection(gI$sel)
		gI$sel <- NULL
	    }

	    if(is.null(gI$sel)) { 
		## point
		gI$sel <- gPointSelection(location)
	    
	    }else if(is(gI$sel, "gPointSelection")) {
		## box
		clearSelection(gI$sel, redraw=FALSE)
		gI$sel <- gBoxSelection(gI$sel, location)
	    }
	}
    }
}


## location helpers
convertLoc <- function(location, unitTo=NULL, valueOnly = FALSE) {
    if(is.null(location)) return(NULL)
    
    if(!is.null(unitTo)) {
	location$x <- convertX(location$x, unitTo)
	location$y <- convertY(location$y, unitTo)
    }

    if(valueOnly) location <- lapply(location, as.numeric)

    location
}

insidePlot <- function(location) all(sapply(convertLoc(location, "npc", 
			valueOnly=TRUE), FUN = function(z) z>=0 && z<=1))
