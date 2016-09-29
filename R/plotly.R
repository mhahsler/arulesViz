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

# plotly interactive plots using d3.js

#plot <- function(x, method = NULL, measure = "support", 
#  shading = "lift", interactive = FALSE, data = NULL, control = NULL, ...) {

# FIXME: also for itemsets and matrix... 

plotly_arules <- function(x, method = "scatterplot", 
  measure = c("support", "confidence"), shading = "lift", 
  max = 1000, ...) {
  
  methods <- c("scatterplot", "matrix")
  
  if(length(x)<1) stop("x contains 0 rules!")
  
  if(is.null(method)) methodNr <- 1
  else methodNr <- pmatch(tolower(method), tolower(methods))
  if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))
  
  # filter by max 
  quality(x)[["order"]] <- size(x) 
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]
  ### order x to prevent overplotting and limit x
  ### FIXME: RuleID
  #o <- order(x, by = )
  
  if(length(x) > max) {
    warning("too many rules supplied only plotting the best ", 
      max, " rules using ", shading, " (change parameter max if needed)")
    x <- tail(x, n = max, by = shading, decreasing = FALSE)
  }else x <- sort(x, by = shading, decreasing = FALSE) 
  
  if(methodNr == 1) .plotly_scatter(x, measure, shading, ...)
  else .plotly_matrix(x, shading, ...)
  
}

.plotly_scatter <- function(x, 
  measure = c("support", "confidence"), shading = "lift", 
  colors = default_colors(100), ...) {

  colors <- rev(colors) 
  
  q <- quality(x)
  l <- labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
    ruleSep = '<BR>&nbsp;&nbsp; &rArr; ', 
    setStart = '<B>{', setEnd = '}</B>')
  
  txt <- paste(paste0('[', 1:length(x),']<br>'), l, 
    paste('<BR><BR>', measure[1], ": ", round(q[, measure[1]], 2), sep = ""),
    paste('<BR>', measure[2], ": ", round(q[, measure[2]], 2), sep =""),
    paste('<BR>', shading, ": ", 
      if(is.numeric(q[, shading])) round(q[, shading], 2) 
      else q[, shading], sep="")
  )

  if(shading == "order")
    p <- plot_ly(q, x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = as.ordered(q[,shading]),
      mode = 'markers', marker = list(...) 
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

.plotly_matrix <- function(x, measure = "lift", reorder = TRUE, 
  #colors = colorRamp(c("grey", "red"))) {
  colors = default_colors(100)) {
  
  colors <- rev(colors)
  m <- rulesAsMatrix(x, measure = measure)
  if(reorder) {
    o <- .reorder(m, method = "TSP")
    m <- seriation::permute(m, o)
  }

  
  txt <- t(outer(colnames(m), rownames(m), paste, sep = ' &rArr; '))
  txt[] <- paste('<B>', txt, '</B>', '<BR>',measure, ': ', round(m, 2), sep = '')
  txt[is.na(m)] <- NA
  
  plot_ly(z = m,
    x = colnames(m), y = rownames(m),
    type = "heatmap",
    colors = colors,
    colorbar = list(title = measure),
    hoverinfo = 'text',
    text = txt
  ) %>% 
    layout(xaxis=list(title="LHS",showticklabels = FALSE, showgrid = FALSE), 
      yaxis=list(title="RHS", showticklabels = FALSE, showgrid = FALSE)
      #,margin=list(l=200, autoexpand=TRUE)
      #yaxis=list(title="RHS", showticklabels = FALSE, showgrid = FALSE)
    )
}


