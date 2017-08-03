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

plot.rules <- function(x, method = NULL, measure = "support", 
  shading = "lift", interactive = FALSE, data = NULL, control = NULL, 
  engine = "default", ...) {
  ## methods
  methods <- c(
    "matrix",       
    "mosaic",
    "doubledecker",
    "graph",
    "paracoord",
    "scatterplot",	## standard
    "grouped",
    "two-key plot",
    "matrix3D",
    "iplots"
  )
  
  if(length(x)<1) stop("x contains 0 rules!")
  
  if(is.null(method)) methodNr <- 6
  else methodNr <- pmatch(tolower(method), tolower(methods))
  if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method), "\nAvailable methods:", paste(methods, collapse = ", ")))
  
  ## add interactive and engine
  if(is.null(control$interactive)) control$interactive <- interactive    
  if(is.null(control$engine)) control$engine <- engine
  
  ## work horses
  if (methodNr == 1) matrix_arules(x, measure = measure, control,...)
  else if (methodNr == 2) doubledecker_arules(x, measure = measure, 
    data = data, c(control, list(type="mosaic")), ...)
  else if (methodNr == 3) doubledecker_arules(x, measure = measure, 
    data = data, control, ...)
  else if (methodNr == 4) graph_arules(x, measure = measure, 
    shading = shading, control, ...)
  else if (methodNr == 5) paracoord_arules(x, measure = measure, 
    shading = shading, control = control,...)
  else if (methodNr == 6) {
    if(length(measure)<2) measure[2] <- "confidence"
    scatterplot_arules(x, measure = measure, 
      shading = shading, control, ...)
  }
  else if (methodNr == 7) grouped_matrix_arules(x, measure= measure, 
    shading = shading, control=control, ...)
  else if (methodNr == 8) scatterplot_arules(x, 
    measure = c("support", "confidence"), shading = "order", 
    control, ...)
  else if (methodNr == 9) matrix_arules(x, measure = measure, 
    control = c(control, list(type="3d")), ...)
  else if (methodNr == 10) {
    if(length(measure)<2) measure[2] <- "confidence"
    iplot_arules(x, measure = measure, 
      shading=shading, data=data, control=control, ...)
  }
}

plot.itemsets <- function(x, method = NULL, measure = "support", shading = NA, interactive=FALSE, data = NULL, control = NULL, engine = "default", ...) {
  ## methods
  methods <- c(
    "graph",
    "paracoord",
    "scatterplot"
  )
  
  if(length(x)<1) stop("x contains 0 itemsets!")
  
  if(is.null(method)) methodNr <- 3
  else methodNr <- pmatch(tolower(method), tolower(methods))
  if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method), "\nAvailable methods:", paste(methods, collapse = ", ")))
  
  ## add interactive
  if(is.null(control$interactive)) control$interactive <- interactive    
  if(is.null(control$engine)) control$engine <- engine
  
  ## work horses
  if (methodNr == 1) graph_arules(x, measure = measure,
    control= control, ...)
  else if (methodNr == 2) paracoord_items(x, measure = measure,
    control= control, ...)
  else if (methodNr == 3) {
    if(length(measure)<2) measure[2] <- "order"
    scatterplot_arules(x, measure = measure, 
      shading = shading, control, ...)
  }
  
}
