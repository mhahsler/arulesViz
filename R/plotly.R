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

# FIXME: also for itemsets 

plotly_arules <- function(x, method = "scatterplot", 
  measure = c("support", "confidence"), shading = "lift", 
  max = 1000, ...) {
  
  if(!is(x, "rules")) stop("x has to be a set of rules.")
  
  methods <- c("scatterplot", "two-key plot", "matrix")
  
  if(length(x)<1) stop("x contains 0 rules!")
  
  if(is.null(method)) methodNr <- 1
  else methodNr <- pmatch(tolower(method), tolower(methods))
  if(is.na(methodNr)) stop (paste("Unknown method:",sQuote(method)))
  
  # two-key plot is a scatterplot with order shading
  if(methodNr==2) {
    shading <- "order"
    methodNr <- 1
  }
  
  quality(x)[["order"]] <- size(x) 
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]
  
  if(methodNr == 1) .plotly_scatter(x, measure, shading, max = max, ...)
  else .plotly_matrix(x, shading, max = max, ...)
}

.plotly_scatter <- function(x, 
  measure = c("support", "confidence"), shading = "lift", 
  colors = default_colors(2), jitter = NA, precision = 3, max = 1000, ...) {

  colors <- rev(colors) 
 
  ### order (overplotting) and check for max 
  o <- order(quality(x)[[shading]], decreasing = FALSE)
  if(length(o) > max) {
    warning("too many rules supplied only plotting the best ", 
      max, " rules using ", shading, " (change parameter max if needed)")
    o <- tail(o, n = max)
  }
  
  x <- x[o]
  q <- quality(x)[, c(measure, shading)]

  for(i in 1:ncol(q)) {
    infin <- is.infinite(q[[i]])
    if(any(infin)) {
      replinfin <- signif(2 * max(q[[i]][!infin], na.rm = TRUE), 3)
      warning(colnames(q)[i], " contains infinite values! Replaced by twice the max (", replinfin, ")!")
      q[[i]][infin] <- replinfin
    }
  } 
    
  l <- labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
    ruleSep = '<BR>&nbsp;&nbsp; => ', 
    setStart = '<B>{', setEnd = '}</B>')
  
  txt <- paste(paste0('[', o,']<BR>'), l, 
    paste('<BR><BR>', measure[1], ": ", signif(q[, measure[1]], precision), sep = ""),
    paste('<BR>', measure[2], ": ", signif(q[, measure[2]], precision), sep =""),
    paste('<BR>', shading, ": ", 
      if(is.numeric(q[, shading])) signif(q[, shading], precision) 
      else q[, shading], sep="")
  )
 
  ### add x/y-jitter
  jitter <- jitter[1]
  if(is.na(jitter) && any(duplicated(q[,measure]))) {
      jitter <- .1
  }
  
  if(!is.na(jitter) && jitter>0) 
    for(m in measure) q[[m]] <- jitter(q[[m]], factor = jitter, amount = 0)

  if(shading == "order")
    p <- plot_ly(q, type = "scatter", x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = as.ordered(q[,shading]),
      mode = 'markers', marker = list(...) 
    ) 
  else
    p <- plot_ly(q, type = "scatter", x = q[,measure[1]], y = q[,measure[2]], 
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
  colors = default_colors(2), precision = 3, max = 1000) {
  
  colors <- rev(colors)
  
  if(length(x) > max) {
    warning("too many rules supplied only plotting the best ", 
      max, " rules using ", measure, " (change parameter max if needed)")
    x <- tail(x, n = max, by = measure, decreasing = FALSE)
  }
  
  m <- rulesAsMatrix(x, measure = measure, itemSep= ',<BR>&nbsp;&nbsp;', 
    setStart = '<B>{', setEnd = '}</B>')
  
  if(reorder) {
    o <- .reorder(m, method = "TSP")
    m <- seriation::permute(m, o)
  }

  
  txt <- t(outer(colnames(m), rownames(m), paste, sep = '<BR>&nbsp;&nbsp; => '))
  txt[] <- paste('<B>', txt, '</B>', '<BR>',measure, ': ', signif(m, precision), sep = '')
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


