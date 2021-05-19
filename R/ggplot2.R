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

scatterplot_ggplot2 <- function(x, 
  measure = c("support", "confidence"), shading = "lift", 
  control = NULL, ...) {
  
  control <- c(control, list(...))  
  control <- .get_parameters(control, list(
    main = paste("Scatter plot for", length(x), class(x)),
    colors = default_colors(2), 
    jitter = NA,
    engine = "ggplot2"
  ))
  
  colors <- rev(control$colors)
  jitter <- control$jitter
  
  q <- quality(x)
  q[["order"]] <- as.factor(size(x)) 
  qnames <- names(q)
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]

  ### order to reduce overplotting 
  if(!is.na(shading)) {
    o <- order(q[[shading]], decreasing = FALSE)
    q <- q[o,, drop = FALSE]
  }
  
  # for(i in 1:ncol(q)) {
  #   infin <- is.infinite(q[[i]])
  #   if(any(infin)) {
  #     replinfin <- signif(2 * max(q[[i]][!infin], na.rm = TRUE), 3)
  #     warning("plot: ", colnames(q)[i], " contains infinite values! Replaced by twice the max (", replinfin, ")!", call. = FALSE)
  #     q[[i]][infin] <- replinfin
  #   }
  # } 
    
  ### add x/y-jitter
  jitter <- jitter[1]
  if(is.na(jitter) && any(duplicated(q[,measure]))) {
    message("To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.")   
    jitter <- .jitter_default
  }
  
  if(!is.na(jitter) && jitter>0) 
    for(m in measure) 
      if(is.numeric(q[[m]])) q[[m]] <- jitter(q[[m]], factor = jitter, amount = 0)

  if(is.na(shading)) shading <- NULL
  p <- ggplot(q, aes_string(measure[1], y = measure[2], color = shading)) +
    geom_point()
  
  if(!is.null(shading)) {
    if(shading != "order")
      p <- p + scale_color_gradient(low=colors[1], high=colors[2])
    else
      p <- p + scale_color_discrete()
  }
  
  p + ggtitle(control$main) + theme_linedraw()
  
}

matrix_ggplot2 <- function(x, 
  measure = c("lift"), shading = NA, control = NULL, ...) {
  
  control <- c(control, list(...))  
  control <- .get_parameters(control, list(
    main = paste("Matrix for", length(x), "rules"),
    colors = default_colors(2), 
    reorder = "measure",
    max = 1000,
    engine = "ggplot2"
  ))
  
  colors <- rev(control$colors)
  
  m <- .reordered_matrix(x, measure, control$reorder)
  dimnames(m) <- list(seq_len(nrow(m)), seq_len(ncol(m)))
  
  # NOTE: nullify variables used for non-standard evaluation for tidyverse/ggplot2 below
  RHS <- LHS  <- NULL
  
  d <- m %>% as_tibble() %>% dplyr::mutate(RHS = seq_len(nrow(m))) %>% 
    pivot_longer(cols = -c(RHS), names_to = "LHS", values_to = measure)  
  d$LHS <- as.integer(d$LHS)
  
  ggplot(d, aes_string(x = 'LHS', y = 'RHS', fill = measure)) + geom_raster() +
    scale_fill_gradient(low=colors[1], high=colors[2], na.value = 0) +
    ggtitle(control$main) + 
    theme_linedraw()
}


graph_ggplot2 <- function(x, measure = "support", shading = "lift", 
  control = NULL, ...) {
  
  if(is.na(shading)) shading <- NULL
  
  ### NULLify stuff for CRAN
  label <- y <- xend <- yend <- NULL
  
  control <- c(control, list(...))  
  control <- .get_parameters(control, list(
    #main = paste("Graph for", length(x), "rules"),
    layout = igraph::nicely(),
    edges = ggnetwork::geom_edges(color = "grey80", 
      arrow = arrow(length = unit(6, "pt"), type = "closed"), alpha = .7),
    nodes = ggnetwork::geom_nodes(aes_string(size = measure, color = shading), na.rm = TRUE),
    nodetext = ggnetwork::geom_nodetext(aes(label = label)),
    colors = default_colors(2), 
    engine = "ggplot2", 
    max = 100
  ))
  
  if(length(x) > control$max) {
    warning("Too many rules supplied. Only plotting the best ", 
      control$max, " rules using ", shading, 
      " (change control parameter max if needed)", call. = FALSE)
    x <- tail(x, n = control$max, by = shading, decreasing = FALSE)
  }
  
  
  g <- associations2igraph(x)
  n <- ggnetwork::fortify(g, layout = control$layout)
  
  ggplot(n, aes(x = x, y = y, xend = xend, yend = yend)) +
    control$edges +
    control$nodes +
    control$nodetext + 
    scale_color_gradient(low = control$colors[2], high = control$colors[1], na.value = 0) +
    ggnetwork::theme_blank()
}

grouped_matrix_ggplot2 <- function(x, 
  measure = c("support"), shading = "lift", control = NULL, ...) {
  
  control <- c(control, list(...))  
  control <- .get_parameters(control, list(
    k = 20,
    rhs_max = 10,
    lhs_items = 2,
    aggr.fun=mean, 
    col = default_colors(2),
    max.shading = NA,
    reverse = TRUE,
    engine = "ggplot2"
  ))
  
  ### get the clustering
  p <- grouped_matrix_int(x, measure, shading, control, plot = FALSE)  

  ## get most important item in the lhs
  f <- lapply(split(p$rules, p$cl), FUN = function(r) itemFrequency(lhs(r), 
    type = "absolute"))
  
  ## divide by sum to find most important item...
  f <- lapply(f, "/", itemFrequency(lhs(p$rules), type = "absolute")+1L)
  
  most_imp_item <- lapply(f, FUN = 
      function(x) {
        items <- sum(x>0)
        if(items==0) { "" }
        else if(control$lhs_items<1){ 
          paste(items, "items") 
        } else if(items>control$lhs_items){
          paste(paste(names(x[head(order(x, decreasing = TRUE), n = control$lhs_items)]), 
            collapse = ", "), ", +", items-control$lhs_items, " items", sep="")
        }else{
          paste(names(x[head(order(x, decreasing = TRUE), n = items)]), 
            collapse = ", ")
        }
      })
  
  s <- p$sAggr
  m <- p$mAggr
  colnames(s) <- paste(
    paste(format(table(p$cl)), " rules: ", '{',most_imp_item, '}', sep=''))
  
  if(control$reverse) p$order[[1]] <- rev(p$order[[1]])
  s <- permute(s, p$order)
  m <- permute(m, p$order)
  
  df <- data.frame(
    LHS = rep(ordered(colnames(s), levels = colnames(s)), times = nrow(s)), 
    RHS = rep(ordered(rownames(s), levels = rownames(s)), each = ncol(s)), 
    support = as.vector(t(m)), 
    measure = as.vector(t(s))
  )

  
  ### NULLify for CRAN
  LHS <- RHS <- NULL
  
  p <- ggplot(df, aes(x = LHS, y = RHS, size = support, color = measure)) +
    geom_point(na.rm = TRUE) + 
    scale_color_gradient(low = control$col[2], high = control$col[1]) + 
    labs(color = p$shading) +
    theme_linedraw() +
    theme(axis.text.x=element_text(angle=90, hjust=0, vjust = .5)) +
    scale_x_discrete(position = "top") 

  if(control$engine == "htmlwidget") p <- plotly::ggplotly(p)
  p
}



