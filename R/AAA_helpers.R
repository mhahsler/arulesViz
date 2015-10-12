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


.installed <- function(pkg) !is(try(installed.packages()[pkg,],
  silent=TRUE), "try-error")

.get_parameters <- function(p, parameter) {
  if(!is.null(parameter) && length(parameter) != 0) {
    
    # get rid of NULL enties
    parameter[sapply(parameter, is.null)] <- NULL
    
    o <- pmatch(names(parameter), names(p))
    
    if(any(is.na(o)))
      stop(sprintf(ngettext(length(is.na(o)),
        "Unknown option: %s",
        "Unknown options: %s"),
        paste(names(parameter)[is.na(o)],
          collapse = " ")))
    
    p[o] <- parameter
  }
  
  p
}


rulesAsDataFrame <- function(rules, measure = "support") {
  antes <- labels(lhs(rules))
  conseqs <- labels(rhs(rules))
  
  data.frame(
    antecedent = factor(antes, levels = unique(antes)),
    consequent = factor(conseqs, levels = unique(conseqs)),
    measure = quality(rules)[[measure]]
  )
}

rulesAsMatrix <- function(rules, measure = "support") {
  df <- rulesAsDataFrame(rules, measure)
  
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
      levels = c(0, 1), labels = c("no", "yes"))
  }
  table(ruleAsDataFrame)
}