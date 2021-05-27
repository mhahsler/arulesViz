#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyright (C) 2021 Michael Hahsler
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
  measure = c("support", "confidence"),
  shading = "lift",
  control = NULL,
  ...) {
  control <- c(control, list(...))
  control <- .get_parameters(control,
    list(
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
  if (!is.na(shading)) {
    o <- order(q[[shading]], decreasing = FALSE)
    q <- q[o, , drop = FALSE]
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
  if (is.na(jitter) && any(duplicated(q[, measure]))) {
    message("To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.")
    jitter <- .jitter_default
  }
  
  if (!is.na(jitter) && jitter > 0)
    for (m in measure)
      if (is.numeric(q[[m]]))
        q[[m]] <- jitter(q[[m]], factor = jitter, amount = 0)
  
  if (is.na(shading))
    shading <- NULL
  p <-
    ggplot(q, aes_string(measure[1], y = measure[2], color = shading)) +
    geom_point()
  
  if (!is.null(shading)) {
    if (shading != "order")
      p <- p + scale_color_gradient(low = colors[1], high = colors[2])
    else
      p <- p + scale_color_discrete()
  }
  
  p + ggtitle(control$main) + theme_linedraw()
  
}

matrix_ggplot2 <- function(x,
  measure = c("lift"),
  shading = NA,
  control = NULL,
  ...) {
  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      main = paste("Matrix for", length(x), "rules"),
      colors = default_colors(2),
      reorder = "measure",
      max = 1000,
      engine = "ggplot2"
    )
  )
  
  colors <- rev(control$colors)
  
  m <- rules2matrix(x, measure, control$reorder)
  
  # reverse rows so highest value is in the top-left hand corner
  m <- m[nrow(m):1,]
  
  ## print labels
  writeLines("Itemsets in Antecedent (LHS)")
  print(colnames(m))
  writeLines("Itemsets in Consequent (RHS)")
  print(rownames(m))
  
  dimnames(m) <- list(seq_len(nrow(m)), seq_len(ncol(m)))
  
  # NOTE: nullify variables used for non-standard evaluation for tidyverse/ggplot2 below
  RHS <- LHS  <- NULL
  
  d <-
    m %>% as_tibble() %>% dplyr::mutate(RHS = seq_len(nrow(m))) %>%
    pivot_longer(cols = -c(RHS),
      names_to = "LHS",
      values_to = measure)
  d$LHS <- as.integer(d$LHS)
  
  ggplot(d, aes_string(x = 'LHS', y = 'RHS', fill = measure)) + geom_raster() +
    scale_fill_gradient(low = colors[1],
      high = colors[2],
      na.value = 0) +
    ggtitle(control$main) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw()
}

grouped_matrix_ggplot2 <- function(x,
  measure = c("support"),
  shading = "lift",
  control = NULL,
  ...) {
  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      k = 20,
      aggr.fun = mean,
      rhs_max = 10,
      lhs_label_items = 2,
      col = default_colors(2),
      #max.shading = NA,
      groups = NULL,
      engine = "ggplot2"
    )
  )
  
  # get the clustering
  if (is.null(control$groups))
    gm <-
    rules2groupedMatrix(
      rules,
      shading,
      measure,
      k = control$k,
      aggr.fun = control$aggr.fun,
      lhs_label_items = control$lhs_label_items
    )
  else
    gm <- control$groups
  control$groupes <- NULL ### for interactive plot
  
  
  m <- gm$m
  m2 <- gm$m2
  
  not_shown_rhs <- 0
  if (nrow(m) > control$rhs_max) {
    not_shown_rhs <- nrow(m) - control$rhs_max
    m <- m[seq_len(control$rhs_max), , drop = FALSE]
    m2 <- m2[seq_len(control$rhs_max), , drop = FALSE]
  }
  
  # convert to data.frame
  df <- data.frame(
    LHS = rep(ordered(colnames(m), levels = colnames(m)), times = nrow(m)),
    RHS = rep(ordered(rownames(m), levels = rev(rownames(
      m
    ))), each = ncol(m)),
    measure = as.vector(t(m)),
    support = as.vector(t(m2))
  )
  
  # NULLify for CRAN
  LHS <- RHS <- NULL
  
  p <-
    ggplot(df, aes(
      x = LHS,
      y = RHS,
      size = support,
      color = measure
    )) +
    geom_point(na.rm = TRUE) +
    scale_color_gradient(low = control$col[2], high = control$col[1]) +
    labs(color = shading) +
    xlab("Items in LHS Groups") +
    ylab(paste(
      "RHS",
      if (not_shown_rhs > 0)
        paste('(+', not_shown_rhs, ' not shown)', sep = '')
      else
        ''
    )) +
    theme_linedraw() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") +
    theme(axis.text.x = element_text(
      angle = 90,
      hjust = 0,
      vjust = .5
    ),
      legend.position = "bottom") +
    scale_size(range = c(2, 8))
  
  if (control$engine == "htmlwidget")
    p <- plotly::ggplotly(p)
  p
}

graph_ggplot2 <- function(x,
  measure = "support",
  shading = "lift",
  control = NULL,
  ...) {
  if (is.na(shading))
    shading <- NULL
  
  ### NULLify stuff for CRAN
  label <- y <- xend <- yend <- NULL
  
  control <- c(control, list(...))
  control <- .get_parameters(
    control,
    list(
      #main = paste("Graph for", length(x), "rules"),
      layout = igraph::nicely(),
      edges = ggnetwork::geom_edges(
        color = "grey80",
        arrow = arrow(length = unit(6, "pt"), type = "closed"),
        alpha = .7
      ),
      nodes = ggnetwork::geom_nodes(aes_string(size = measure, color = shading), na.rm = TRUE),
      nodetext = ggnetwork::geom_nodetext(aes(label = label)),
      colors = default_colors(2),
      engine = "ggplot2",
      max = 100
    )
  )
  
  if (length(x) > control$max) {
    warning(
      "Too many rules supplied. Only plotting the best ",
      control$max,
      " rules using ",
      shading,
      " (change control parameter max if needed)",
      call. = FALSE
    )
    x <- tail(x,
      n = control$max,
      by = shading,
      decreasing = FALSE)
  }
  
  
  g <- associations2igraph(x)
  n <- ggnetwork::fortify(g, layout = control$layout)
  
  ggplot(n, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
    control$edges +
    control$nodes +
    control$nodetext +
    scale_color_gradient(low = control$colors[2],
      high = control$colors[1],
      na.value = 0) +
    ggnetwork::theme_blank()
}
