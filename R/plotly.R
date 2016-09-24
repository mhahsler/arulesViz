#######################################################################
# axViz - Visualizing Association x and Frequent Itemsets
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

# plotly interactive plots using d3.js

#plot <- function(x, method = NULL, measure = "support", 
#  shading = "lift", interactive = FALSE, data = NULL, control = NULL, ...) {


plotly_arules <- function(x, measure = c("support", "confidence"), 
  shading = "lift", max = 1000, colors = colorRamp(c("grey", "red")),
  size=7, opacity = .8, ...) {
  
  quality(x)[["order"]] <- size(lhs(x)) 
  
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]
  
  ### order x to prevent overplotting and limit x
  ### FIXME: RuleID
  #o <- order(x, by = )
  
  if(length(x) > max) {
    warning("too many x supplied only plotting the best ", 
      max, " x using ", shading, " (change parameter max if needed)")
    x <- tail(x, n = max, by = shading, decreasing = FALSE)
    
  }else x <- sort(x, by = shading, decreasing = FALSE) 
 
  q <- quality(x)
   
  #l <- labels(x)
  l <- labels(x, itemSep= ",<br>&nbsp;", ruleSep = "<br> &rArr; ", 
    setStart = "{", setEnd = "}")
  
  txt <- paste(l, 
    paste("<br><br>", measure[1], ":", round(q[, measure[1]], 2), sep = ""),
    paste(measure[2], ":", round(q[, measure[2]], 2), sep =""),
    paste(shading, ": ", 
      if(is.numeric(q[, shading])) round(q[, shading], 2) 
      else q[, shading], sep="")
  )

  if(shading == "order")
    p <- plot_ly(q, x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = as.ordered(q[,shading]),
      mode = 'markers' 
    ) 
  else
    p <- plot_ly(q, x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = q[,shading], colors = colors,
      mode = 'markers', 
      marker = list(colorbar = list(title = shading), ...)
    ) 
  
  
  p %>% layout(hovermode = "closest", 
    xaxis = list(title = measure[1]),
    yaxis = list(title = measure[2])
  )
}

