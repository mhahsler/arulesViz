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


plotly_arules <- function(x, method = "scatterplot", 
  measure = c("support", "confidence"), shading = "lift", 
  max = 1000, ...) {
  
  .Deprecated("plot")

  .plotly_arules(x, method, measure, shading, 1000, ...)
}

.plotly_arules <- function(x, method = "scatterplot", 
  measure = c("support", "confidence"), shading = "lift", 
  max = 1000, ...) {
  
  if(!is(x, "associations")) stop("x has to be a set of rules.")
  
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
  
  #quality(x)[["order"]] <- size(x) 
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]
  
  if(methodNr == 1) 
    .plotly_scatter(x, measure, shading, max = max, ...)
  else 
    .plotly_matrix(x, shading, max = max, ...)
}

## Interface used by plot
scatterplot_plotly <- function(x,
  measure = measure, shading = shading, control = control, ...) {
  
  control <- c(control, list(...))  
  
  control <- .get_parameters(control, list(
    interactive = TRUE,
    engine = "htmlwidget",
    max = 1000,
    colors = default_colors(2), 
    jitter = NA, 
    precision = 3,
    main = "Unused",
    marker = list()
  ))
  
  quality(x)[["order"]] <- size(x) 
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]
 
  .plotly_scatter(x, measure, shading, control$colors, control$jitter, control$precision, control$max, control$marker)
}

.plotly_scatter <- function(x, 
  measure = c("support", "confidence"), shading = "lift", 
  colors = default_colors(2), jitter = NA, precision = 3, 
  max = 1000, marker = list(), ...) {

  colors <- rev(colors) 
 
  ### order (overplotting) and check for max 
  if(!is.na(shading)) o <- order(quality(x)[[shading]], decreasing = FALSE)
  else o <- 1:length(x)
  
  if(length(o) > max) {
    warning("plot: Too many rules supplied. Only plotting the best ", 
      max, " rules using measure ", shading, 
      " (change parameter max if needed)", call. = FALSE)
    o <- tail(o, n = max)
  }
  
  x <- x[o]
  if(!is.na(shading)) q <- quality(x)[, c(measure, shading)]
  else q <- quality(x)[, measure]
  
  for(i in 1:ncol(q)) {
    infin <- is.infinite(q[[i]])
    if(any(infin)) {
      replinfin <- signif(2 * max(q[[i]][!infin], na.rm = TRUE), 3)
      warning("plot: ", colnames(q)[i], " contains infinite values! Replaced by twice the max (", replinfin, ")!", call. = FALSE)
      q[[i]][infin] <- replinfin
    }
  } 
    
  if(is(x, "rules")) l <- labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
    ruleSep = '<BR>&nbsp;&nbsp; => ', 
    setStart = '<B>{', setEnd = '}</B>')
  else l <- labels(x, itemSep= ',<BR>&nbsp;&nbsp;', 
    setStart = '<B>{', setEnd = '}</B>')
  
  
  txt <- paste(paste0('[', o,']<BR>'), l, 
    paste('<BR><BR>', measure[1], ": ", signif(q[, measure[1]], precision), sep = ""),
    paste('<BR>', measure[2], ": ", signif(q[, measure[2]], precision), sep =""),
    if(!is.na(shading)){ 
      paste('<BR>', shading, ": ", 
        if(is.numeric(q[, shading])) signif(q[, shading], precision) 
        else q[, shading], sep="")
      } else "" 
  )
  
  ### add x/y-jitter
  jitter <- jitter[1]
  if(is.na(jitter) && any(duplicated(q[,measure]))) {
    message("To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.")   
    jitter <- .1
  }
  
  if(!is.na(jitter) && jitter>0) 
    for(m in measure) q[[m]] <- jitter(q[[m]], factor = jitter)

  if(is.na(shading)) 
    p <- plot_ly(q, type = "scatter", x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt,
      mode = 'markers', marker = marker 
    ) 
  else if(shading == "order")
    p <- plot_ly(q, type = "scatter", x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = as.ordered(q[,shading]),
      mode = 'markers', marker = marker 
    )
  else
    p <- plot_ly(q, type = "scatter", x = q[,measure[1]], y = q[,measure[2]], 
      hoverinfo = 'text', text = txt, 
      color = q[,shading], colors = colors,
      mode = 'markers', 
      marker = marker
    )  %>% plotly::colorbar(title = shading) 
  
  
  p %>%
    plotly::layout(hovermode = "closest", 
      xaxis = list(title = measure[1]),
      yaxis = list(title = measure[2])
    )
}

## interface for plot
matrix_plotly <- function(x, measure, shading, control, ...) {
  
  control <- c(control, list(...))  
  
  control <- .get_parameters(control, list(
    interactive = TRUE,
    engine = "htmlwidget",
    max = 1000,
    colors = default_colors(2), 
    reorder = "measure",
    precision = 3
  ))
  
  qnames <- names(quality(x))
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  
  .plotly_matrix(x, measure[1], reorder = control$reorder, 
    colors = control$colors, precision = control$precision, max = control$max) }

.plotly_matrix <- function(x, measure = "lift", reorder = "none", 
  #colors = colorRamp(c("grey", "red"))) {
  colors = default_colors(2), precision = 3, max = 1000) {
  
  colors <- rev(colors)
  
  if(length(x) > max) {
    warning("plot: Too many rules supplied. Only plotting the best ", 
      max, " rules using ", measure, " (change parameter max if needed)", 
      call. = FALSE)
    x <- tail(x, n = max, by = measure, decreasing = FALSE)
  }
  
  m <- rulesAsMatrix(x, measure = measure, itemSep= ',<BR>&nbsp;&nbsp;', 
    setStart = '<B>{', setEnd = '}</B>')
  m_s <- rulesAsMatrix(x, "support")
  m_c <- rulesAsMatrix(x, "confidence")
  
  reorderTypes <- c("none", "measure", "support/confidence", "similarity")
  reorderType <- pmatch(reorder , reorderTypes, nomatch = 0)
  if(reorderType == 0) stop("Unknown reorder method: ", sQuote(reorder), 
    " Valid reorder methods are: ", paste(sQuote(reorderTypes), 
      collapse = ", "))
  if(reorderType == 2){
    cm <- order(colMeans(m, na.rm = TRUE), decreasing = FALSE)
    rm <- order(rowMeans(m, na.rm = TRUE), decreasing = FALSE)
    m <- m[rm, cm]
    m_s <- m_s[rm, cm]
    m_c <- m_c[rm, cm]
  } else if(reorderType == 3){
    cm <- order(colMeans(m_s, na.rm = TRUE), decreasing = FALSE)
    rm <- order(rowMeans(m_c, na.rm = TRUE), decreasing = FALSE)
    m <- m[rm, cm]
    m_s <- m_s[rm, cm]
    m_c <- m_c[rm, cm]
  } else if(reorderType == 4){
    d <- dissimilarity(lhs(x), method = "jaccard")
    cm <- get_order(seriate(d))
    rm <- order(rowMeans(m, na.rm = TRUE), decreasing = FALSE)
    m <- m[rm, cm]
    m_s <- m_s[rm, cm]
    m_c <- m_c[rm, cm]
  } 

  
  txt <- t(outer(colnames(m), rownames(m), paste, sep = '<BR>&nbsp;&nbsp; => '))
  txt[] <- paste('<B>', txt, '</B>', 
    '<BR>',measure, ': ', signif(m, precision), 
    '<BR>','support', ': ', signif(m_s, precision), 
    '<BR>','confidence', ': ', signif(m_c, precision), 
    sep = '')
  txt[is.na(m)] <- NA
  
  plot_ly(z = m,
    x = colnames(m), y = rownames(m),
    type = "heatmap",
    colors = colors,
    colorbar = list(title = measure),
    hoverinfo = 'text',
    text = txt
  ) %>% 
    layout(xaxis=list(title="LHS", showticklabels = FALSE, 
      showgrid = TRUE, ticks = ""), 
      yaxis=list(title="RHS", showticklabels = FALSE, 
        showgrid = TRUE, ticks = "")
      #,margin=list(l=200, autoexpand=TRUE)
      #yaxis=list(title="RHS", showticklabels = FALSE, showgrid = FALSE)
    )
}


