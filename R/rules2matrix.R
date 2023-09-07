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

#' Convert association rules into a matrix
#' 
#' Converts a set of association rules into a matrix with unique LHS itemsets
#' as columns and unique RHS itemsets as rows. The matrix cells contain a
#' quality measure. The LHS itemsets can be grouped.
#' 
#' 
#' @aliases rules2matrix rules2groupedMatrix
#' @param rules a rules object.
#' @param measure quality measure put in the matrix
#' @param reorder reorder rows and columns? Possible methods are: "none",
#' "measure" (default), "support/confidence", "similarity".
#' @param measure2 second quality measure (organized in the same way as
#' measure).
#' @param k number of LHS itemset groups.
#' @param aggr.fun function to aggregate the quality measure for groups.
#' @param lhs_label_items number of top items used to name LHS itemset groups
#' (columns).
#' @param ...  passed on to [`DATAFRAME()`].
#' @return \code{rules2matrix} returns a matrix with quality values.
#' 
#' \code{rules2groupedMatrix} returns a list with elements 
#' \item{m}{ the
#' grouped matrix for measure. } 
#' \item{m2}{ the grouped matrix for measure2. }
#' \item{clustering_rules}{ vector with group assignment for each rule. }
#' @author Michael Hahsler
#' @seealso \code{\link{plot}} for rules using \code{method = 'matrix'} and
#' \code{method = 'grouped matrix'}.
#' @references Michael Hahsler and Radoslaw Karpienko. Visualizing association
#' rules in hierarchical groups. Journal of Business Economics, 87(3):317--335,
#' May 2016. \doi{10.1007/s11573-016-0822-8}.
#' @examples
#' 
#' data(Groceries)
#' rules <- apriori(Groceries, parameter=list(support = 0.001, confidence = 0.8))
#' rules
#' 
#' ## Matrix
#' m <- rules2matrix(rules[1:10], measure = "lift")
#' m
#' plot(rules[1:10], method = "matrix")
#' 
#' ## Grouped matrix
#' # create a matrix with LHSs grouped in k = 10 groups
#' gm <- rules2groupedMatrix(rules, k = 10)
#' gm$m
#' 
#' # number of rules per group 
#' table(gm$clustering_rules)
#' 
#' # get rules for group 1
#' inspect(rules[gm$clustering_rules == 1])
#' 
#' # create the corresponding grouped matrix plot by passing the grouped matrix as the groups parameter
#' plot(rules, method = "grouped matrix", groups = gm)
#' 
#' @export
rules2matrix <-
  function(rules,
    measure = "support",
    reorder = "measure",
    ...) {
    df <- DATAFRAME(rules, ...)
    
    m <- matrix(
      NA,
      nrow = length(levels(df$RHS)),
      ncol = length(levels(df$LHS)),
      dimnames = list(levels(df$RHS), levels(df$LHS))
    )
    
    # attribute encoding contains the rule ids
    enc <- m
  
    if(is.null(measure)) stop("shading measure needed for this visualization")
     
    for (i in seq_len(nrow(df))) {
      m[df$RHS[i], df$LHS[i]] <- df[[measure]][i]
      enc[df$RHS[i], df$LHS[i]] <- i
    }
    
    
    # reorder
    reorderTypes <-
      c("none", "measure", "support/confidence", "similarity")
    reorderType <- pmatch(reorder , reorderTypes, nomatch = 0)
    if (reorderType == 0)
      stop(
        "Unknown reorder method: ",
        sQuote(reorder),
        " Valid reorder methods are: ",
        paste(sQuote(reorderTypes),
          collapse = ", ")
      )
    
    if (reorderType == 2) {
      cm <- colMeans(m, na.rm = TRUE)
      rm <- rowMeans(m, na.rm = TRUE)
      m <-
        m[order(rm, decreasing = TRUE), order(cm, decreasing = TRUE)]
      enc <-
        enc[order(rm, decreasing = TRUE), order(cm, decreasing = TRUE)]
      
    } else if (reorderType == 3) {
      cm <- colMeans(rules2matrix(rules, "support"), na.rm = TRUE)
      rm <- rowMeans(rules2matrix(rules, "confidence"), na.rm = TRUE)
      m <-
        m[order(rm, decreasing = TRUE), order(cm, decreasing = TRUE)]
      enc <-
        enc[order(rm, decreasing = TRUE), order(cm, decreasing = TRUE)]
      
    } else if (reorderType == 4) {
      ### Note: I hope unique is stable and gives the same order as rules2matrix!
      d <- dissimilarity(unique(lhs(rules)), method = "jaccard")
      cm <- seriation::get_order(seriation::seriate(d))
      rm <- rowMeans(m, na.rm = TRUE)
      m <- m[order(rm, decreasing = FALSE), cm]
      enc <- enc[order(rm, decreasing = FALSE), cm]
    }
    
    attr(m, "encoding") <- enc
    
    m
  }

matrixplot <-
  function(rules,
    measure = "lift",
    control = NULL,
    ...) {
    engines <-
      c("default",
        "ggplot2",
        "grid",
        "interactive",
        "base",
        "3d",
        "plotly",
        "htmlwidget")
    if (control$engine == "help") {
      message("Available engines for this plotting method are:\n",
        paste0(engines, collapse = ", "))
      return(invisible(engines))
    }
    
    m <- pmatch(control$engine, engines, nomatch = 0)
    if (m == 0)
      stop(
        "Unknown engine: ",
        sQuote(control$engine),
        " Valid engines: ",
        paste(sQuote(engines), collapse = ", ")
      )
    control$engine <- engines[m]
    
    ### FIXME: fix max and control & reorder!
    if (pmatch(control$engine, c("plotly", "htmlwidget"), nomatch = 0) >
        0) {
      return(matrix_plotly(rules, measure = measure, control = control, ...))
      
    } else if (pmatch(control$engine,
      c("grid", "interactive", "3d", "base"),
      nomatch = 0) > 0) {
      return(matrix_grid(rules, measure = measure, control = control, ...))
    }
    
    ### ggplot is default
    return(matrix_ggplot2(rules, measure = measure, control = control))
  }

### this also does 3d and base
matrix_grid <-
  function(rules,
    measure = "lift",
    control = NULL,
    ...) {
    control <- c(control, list(...))
    control <- .get_parameters(
      control,
      list(
        main = paste("Matrix for", length(rules), "rules"),
        engine = "default",
        col = default_colors(100),
        zlim = NULL,
        axes = TRUE,
        reorder = "measure",
        newpage = TRUE,
        plot_options = list()
      )
    )
    
    ## somehow the colors are reversed
    control$col <- rev(control$col)
    
    ## regular case (only one measure)
    if (length(measure) < 2)
      ret <- matrix_int(rules, measure, control, ...)
    else
      ret <- matrix_int2(rules, measure, control)
    
    if (control$engine != "interactive")
      return(invisible())
    
    ## interactive mode
    cat(
      "Interactive mode.\nIdentify rules by selecting them.\nEnd interactive mode by clicking outside the plotting area!\n"
    )
    
    ## go to viewport
    downViewport("image")
    
    ## no buttons
    gI <- gInteraction()
    
    while (TRUE) {
      gI <- gIdentify(gI)
      
      sel <- selection(gI)
      if (is.null(sel))
        return(invisible())
      
      select <- convertLoc(selection(gI)$loc,
        "native", valueOnly = TRUE)
      select <- lapply(select, round)
      
      rule <- ret[select$y, select$x, drop = FALSE]
      
      if (is.na(as.numeric(rule)))
        cat("No rules selected!\n")
      else
        cat(
          colnames(rule),
          " -> ",
          rownames(rule),
          " (",
          measure[1],
          ": ",
          as.numeric(rule),
          ")\n",
          sep = ''
        )
    }
    
  }

matrix_int <- function(rules, measure, control, ...) {
  m <- rules2matrix(rules, measure, reorder = control$reorder)
  
  # reverse rows so highest value is in the top-left hand corner
  m <- m[nrow(m):1,]
  
  ## print labels
  writeLines("Itemsets in Antecedent (LHS)")
  print(colnames(m))
  writeLines("Itemsets in Consequent (RHS)")
  print(rownames(m))
  
  
  if (control$engine == "base") {
    do.call(image, c(
      list(
        t(m),
        col = control$col,
        xlab = "Antecedent (LHS)",
        ylab = "Consequent (RHS)",
        main = control$main,
        sub = paste("Measure:", measure),
        axes = FALSE
      ),
      control$plot_options
    ))
    if (control$axes) {
      axis(1,
        labels = 1:ncol(m),
        at = (0:(ncol(m) - 1)) / (ncol(m) - 1))
      axis(2,
        labels = 1:nrow(m),
        at = (0:(nrow(m) - 1)) / (nrow(m) - 1))
    }
    box()
  }
  else if (control$engine == "3d") {
    df <- cbind(which(!is.na(m), arr.ind = TRUE), as.vector(m[!is.na(m)]))
    do.call(scatterplot3d::scatterplot3d,
      c(
        list(
          df,
          zlab = measure,
          xlab = "Consequent (RHS)",
          ylab = "Antecedent (LHS)",
          main = control$main,
          type = "h",
          pch = ""
        ),
        control$plot_options
      ))
  }
  else {
    ### this is grid
    #dimnames(m) <- NULL
    #plot(levelplot(t(m), xlab = "Antecedent (LHS)",
    #		ylab = "Consequent (RHS)",
    #		main = control$main, aspect = "fill",
    #		cuts = 20, col.regions = control$col,
    #		sub=paste("Measure:", measure), ...))
    
    ## start plot
    if (control$newpage)
      grid.newpage()
    
    ## main
    gTitle(control$main)
    
    ## image
    pushViewport(viewport(
      x = unit(4, "lines"),
      y = unit(4, "lines"),
      height = unit(1, "npc") - unit(4 + 4, "lines"),
      width = unit(1, "npc") - unit(4 + 2 + 2 + 3, "lines"),
      just = c("left", "bottom")
    ))
    
    if (is.null(control$zlim))
      control$zlim <- range(m, na.rm = TRUE)
    cols <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
    cols[] <- control$col[map(m, c(1, (length(control$col) + 1)),
      from.range = control$zlim)]
    cols[is.na(cols)] <- control$col[length(control$col)]
    cols[is.na(m)] <- NA
    
    do.call(gImage, c(
      list(
        cols,
        xlab = "Antecedent (LHS)",
        ylab = "Consequent (RHS)",
        name = "image",
        axes = "integer"
      ),
      control$plot_options
    ))
    
    upViewport(1)
    
    ### color key
    pushViewport(
      viewport(
        x = unit(1, "npc") - unit(4 + 2, "lines"),
        #y=unit(4, "lines"),
        y = unit(1, "npc") - unit(4, "lines"),
        height = unit(1, "npc") - unit(4 + 4, "lines"),
        width = unit(1, "lines"),
        #just = c("left", "bottom")))
        just = c("left", "top")
      )
    )
    
    gColorkey(control$zlim, control$col, label = measure[1])
    
    upViewport(1)
  }
  
  m
}



## 2 measures
matrix_int2 <- function(rules, measure, control) {
  m1 <- rules2matrix(rules, measure[1])
  m2 <- rules2matrix(rules, measure[2])
  
  ### FIXME: This does not work anymore!!!
  if (control$reorder == TRUE)
  {
    if (is.null(control$reorderBy))
      m_reorder <- m1
    else if (control$reorderBy == measure[1])
      m_reorder <- m1
    else if (control$reorderBy == measure[2])
      m_reorder <- m2
    else
      m_reorder <- rules2matrix(rules, control$reorderBy)
    
    order <-
      .reorder(
        m_reorder,
        rules,
        method = control$reorderMethod,
        control = control$reorderControl
      )
    
    m1 <- seriation::permute(m1, order)
    m2 <- seriation::permute(m2, order)
    
  }
  
  writeLines("Itemsets in Antecedent (LHS)")
  print(colnames(m1))
  writeLines("Itemsets in Consequent (RHS)")
  print(rownames(m1))
  
  ## start plot
  grid.newpage()
  
  ## main
  pushViewport(viewport(
    y = 1,
    height = unit(4, "lines"),
    just = c("top")
  ))
  grid.text(control$main,
    gp = gpar(fontface = "bold", cex = 1.2))
  upViewport(1)
  
  ## image
  pushViewport(viewport(
    x = unit(4, "lines"),
    y = unit(4, "lines"),
    height = unit(1, "npc") - unit(4 + 4, "lines"),
    width = unit(1, "npc") - unit(4 + 2 + 9, "lines"),
    just = c("left", "bottom")
  ))
  
  
  ## h = 0..360, but we only use 0..260
  ## l = 0..100 but we use 10..90
  ## all colors are reversed
  
  cols <- matrix(grDevices::hcl(
    h = floor(map(m1, c(260, 0))),
    l = floor(map(m2, c(100, 30))),
    c = floor(map(m2, c(30, 100)))
  ),
    ncol = ncol(m1))
  cols[is.na(m1) | is.na(m2)] <- NA
  
  gImage(
    cols,
    xlab = "Antecedent (LHS)",
    ylab = "Consequent (RHS)",
    name = "image",
    axes = "integer"
  )
  
  upViewport(1)
  
  ### color key
  pushViewport(viewport(
    x = unit(1, "npc") - unit(9 - 3, "lines"),
    #y=unit(4, "lines"),
    y = unit(1, "npc") - unit(4, "lines"),
    #height=unit(1,"npc")-unit(4+4, "lines"),
    height = unit(5, "lines"),
    width = unit(5, "lines"),
    #just = c("left", "bottom")))
    just = c("left", "top")
  ))
  
  
  
  steps <- 10
  mm <-
    outer(
      seq(260, 0, length.out = steps),
      seq(100, 30, length.out = steps),
      FUN = function(x, y)
        grDevices::hcl(h = x, l = y, c = 130 - y)
    )
  
  gImage(
    mm,
    xScale = range(m2, na.rm = TRUE),
    yScale = range(m1, na.rm = TRUE),
    xlab = measure[2],
    ylab = NULL
  )
  
  ## we have to move the label for the y axis out some more
  grid.text(measure[1], unit(-4, "lines"), 0.5, rot = 90)
  upViewport(1)
  
  m1
}


## reorder helper
.reorder <- function(m,
  rules = NULL,
  method = NULL,
  control = NULL) {
  ## rules is only needed by ConfSupp
  
  distMethods <- c("ARSA",
    "BBURCG",
    "BBWRCG",
    "TSP",
    "Chen",
    "MDS",
    "HC",
    "GW",
    "OLO")
  
  if (is.null(method))
    method <- "TSP"
  
  dist <- control$reorderDist
  if (is.null(dist))
    dist <- "euclidean"
  
  ## replace unknown values with 0. Also takes care of NAs (see below)
  m[is.na(m)] <- 0
  
  if (toupper(method) %in% distMethods) {
    l <- dist(m, method = dist)
    r <- dist(t(m), method = dist)
    ## handle NAs make them a large distance
    #l[is.na(l)] <- max(l, na.rm=TRUE) * 2
    #r[is.na(r)] <- max(r, na.rm=TRUE) * 2
    
    ls <- seriation::seriate(l, method = method, control = control)
    rs <- seriation::seriate(r, method = method, control = control)
    return(c(ls, rs))
  } else{
    if (method == "ConfSupp")
    {
      ms <- rules2matrix(rules, "support")
      mc <- rules2matrix(rules, "confidence")
      o1 <- order(colMeans(ms, na.rm = TRUE))
      o2 <- order(rowMeans(mc, na.rm = TRUE))
      o <- seriation::ser_permutation(o2, o1)
      return(o)
      
    } else{
      l <- seriation::seriate(m, method = method, control = control)
      return(l)
    }
  }
  
}

seriation_method_avgMeasure <- function(x, control) {
  seriation::ser_permutation(order(rowMeans(x, na.rm = TRUE)),
    order(colMeans(x, na.rm = TRUE)))
}

seriation_method_maxMeasure <- function(x, control) {
  seriation::ser_permutation(order(apply(
    x, MARGIN = 1, max, na.rm = TRUE
  )),
    order(apply(
      x, MARGIN = 2, max, na.rm = TRUE
    )))
}

seriation_method_medMeasure <- function(x, control) {
  seriation::ser_permutation(order(apply(
    x, MARGIN = 1, median, na.rm = TRUE
  )),
    order(apply(
      x, MARGIN = 2, median, na.rm = TRUE
    )))
}


seriation::set_seriation_method("matrix",
  "avg",
  seriation_method_avgMeasure,
  "Order by average")
seriation::set_seriation_method("matrix",
  "max",
  seriation_method_maxMeasure,
  "Order by maximum")
seriation::set_seriation_method("matrix",
  "median",
  seriation_method_maxMeasure,
  "Order by median")
