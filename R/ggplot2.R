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
    jitter <- .1
  }
  
  if(!is.na(jitter) && jitter>0) 
    for(m in measure) q[[m]] <- jitter(q[[m]], factor = jitter)

  if(is.na(shading)) shading <- NULL
  p <- ggplot(q, aes_string(measure[1], y = measure[2], color = shading)) +
    geom_point()
  
  if(!is.null(shading)) {
    if(shading != "order")
      p <- p + scale_color_gradient(low=colors[1], high=colors[2])
    else
      p <- p + scale_color_discrete()
  }
  p
  
}

matrix_ggplot2 <- function(x, 
  measure = c("lift"), shading = NA, control = NULL, ...) {
  
  control <- c(control, list(...))  
  control <- .get_parameters(control, list(
    colors = default_colors(2), 
    reorder = "measure",
    max = 1000,
    engine = "ggplot2"
  ))
  
  colors <- rev(control$colors)
  
  q <- quality(x)
  q[["order"]] <- size(x) 
  qnames <- names(q)
  measure <- qnames[pmatch(measure, qnames, duplicates.ok = TRUE)]
  shading <- qnames[pmatch(shading, qnames)]

  if(length(x) > control$max) {
    warning("plot: Too many rules supplied. Only plotting the best ", 
      control$max, " rules using ", measure, " (change parameter max if needed)", 
      call. = FALSE)
    x <- tail(x, n = control$max, by = measure, decreasing = FALSE)
  }
 
  measure <- measure[1]
  cat(measure) 
  
  
  m <- rulesAsMatrix(x, measure = measure)
  m_s <- rulesAsMatrix(x, "support")
  m_c <- rulesAsMatrix(x, "confidence")
  
  reorderTypes <- c("none", "measure", "support/confidence", "similarity")
  reorderType <- pmatch(control$reorder , reorderTypes, nomatch = 0)
  if(reorderType == 0) stop("Unknown reorder method: ", sQuote(control$reorder), 
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

  writeLines("Itemsets in Antecedent (LHS)")
  print(colnames(m))
  writeLines("Itemsets in Consequent (RHS)")
  print(rownames(m))
  
  # txt <- t(outer(colnames(m), rownames(m), paste, sep = '<BR>&nbsp;&nbsp; => '))
  # txt[] <- paste('<B>', txt, '</B>', 
  #   '<BR>',measure, ': ', signif(m, precision), 
  #   '<BR>','support', ': ', signif(m_s, precision), 
  #   '<BR>','confidence', ': ', signif(m_c, precision), 
  #   sep = '')
  # txt[is.na(m)] <- NA
 
  dimnames(m) <- list(seq_len(nrow(m)), seq_len(ncol(m)))
   
  # NOTE: nullify variables used for non-standard evaluation for tidyverse/ggplot2 below
  RHS <- LHS <- value <- NULL
  
  d <- m %>% as_tibble() %>% dplyr::mutate(RHS = seq_len(nrow(m))) %>% 
    pivot_longer(cols = -c(RHS), names_to = "LHS")  
  d$LHS <- as.integer(d$LHS)
  
  ggplot(d, aes_string(x = 'LHS', y = 'RHS', fill = 'value')) + geom_tile() +
    scale_fill_gradient(low=colors[1], high=colors[2], na.value = 0) 
}


