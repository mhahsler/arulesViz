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


.installed <- function(pkg) !inherits(try(utils::installed.packages()[pkg,],
  silent=TRUE), "try-error")

## color paletts
.col_picker <- function(level, palette, alpha=NULL) {
  col <- palette[floor(level*(length(palette)-1))+1]
  if(!is.null(alpha)) {
    col <- apply(sapply(col, col2rgb)/255, 2, 
      function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))
  }
  col
}

grey_hcl <- function(n, alpha = 1) colorspace::sequential_hcl(n, c.=0, alpha = alpha)

### default are gray - > red
default_colors <- function(n , alpha = 1) 
  colorRampPalette(c("#EE0000", "#EE9999","#EEEEEE"), alpha = alpha)(n)

## helpers for various visualization
## returns all NAs for unknown measure
rulesAsDataFrame <- function(rules, measure = "support", ...) {
  antes <- labels(lhs(rules), ...)
  conseqs <- labels(rhs(rules), ...)
  qual <- quality(rules)[[measure]]
  if(is.null(qual)) qual <- NA
  
  data.frame(
    antecedent = factor(antes, levels = unique(antes)),
    consequent = factor(conseqs, levels = unique(conseqs)),
    measure = qual
  )
}

# attribute encoding contains the rule ids
rulesAsMatrix <- function(rules, measure = "support", ...) {
  df <- rulesAsDataFrame(rules, measure, ...)
  
  antes <- as.integer(df$antecedent)
  conseqs <- as.integer(df$consequent)
  m <- matrix(NA,
    ncol = length(unique(antes)), nrow = length(unique(conseqs)))
  dimnames(m) <- list(levels(df$consequent), levels(df$antecedent))
  
  enc <- m
  
  for (i in 1:nrow(df)) {
    m[conseqs[i], antes[i]] <- df$measure[i]
    enc[conseqs[i], antes[i]] <- i
  }
  
  attr(m, "encoding") <- enc
  m
}


rulesAStable <- function(rules, data) {
  tables <- list()
  for (i in 1:length(rules)) {
    tables[[i]] <- getTable(rules[i], data)
  }
  tables
}


getTable <- function(rules, data) {
  antecedent <- unlist(LIST(lhs(rules), decode = FALSE))
  consequent <- unlist(LIST(rhs(rules), decode = FALSE))
  transactions <- data[, c(antecedent, consequent)]
  ruleAsDataFrame <- as.data.frame(as(transactions, "matrix"))
  for (i in 1:ncol(ruleAsDataFrame)) {
    ruleAsDataFrame[[i]] <- factor(ruleAsDataFrame[[i]],
      levels = c(FALSE, TRUE), labels = c("no", "yes"))
  }
  table(ruleAsDataFrame)
}

