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

#' Inspect Associations Interactively Using datatable
#'
#' Uses \pkg{datatable} to create a HTML table widget using the DataTables
#' library. Rules can be interactively filtered and sorted.
#'
#' @aliases inspect inspectDT datatable
#' @param x an object of class "rules" or "itemsets".
#' @param precision controls the precision used to print the quality measures (defaults to 2).
#' @param ...  additional arguments are passed on to [`DT::datatable()`].
#' @return A datatable htmlwidget.
#' @author Michael Hahsler
#' @seealso [DT::datatable()] in \pkg{DT}.
#' @references Hahsler M (2017). arulesViz: Interactive Visualization of
#' Association Rules with R. *R Journal,* 9(2):163-175. ISSN 2073-4859.
#' \doi{10.32614/RJ-2017-047}.
#' @keywords print
#' @examplesif requireNamespace("datatable", quietly = TRUE)
#' data(Groceries)
#' rules <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.5))
#' rules
#'
#' inspectDT(rules)
#'
#' # for more control on the data table, you can used DATAFRAME() to convert the rules.
#' rules_df <- DATAFRAME(rules, setStart = "", setEnd = "", itemSep = " + ")
#' rules_df$count <- NULL
#' head(rules_df)
#' inspectDT(rules_df)
#'
#' # Save HTML widget as web page
#' p <- inspectDT(rules)
#' htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
#' # Note: self-contained seems to make the browser slow.
#'
#' # inspect the widget
#' browseURL("arules.html")
#'
#' # clean up
#' unlink(c("arules.html", "arules_files"), recursive = TRUE)
#' @export
inspectDT <- function(x, ...) {
  UseMethod("inspectDT", x)
}

#' @rdname inspectDT
#' @export
inspectDT.default <- function(x, ...) {
  stop("inspect 2 not implemented for class ", class(x))
}

#' @rdname inspectDT
#' @export
inspectDT.rules <- function(x, precision = 3, ...) {
  df <- DATAFRAME(x)
  DT::datatable(df,
    filter = "top",
    rownames = paste0("[", rownames(df), "]"),
    ...
  ) %>%
    DT::formatRound(columns = which(sapply(df, is.numeric)), digits = max(precision))
}

#' @rdname inspectDT
#' @export
inspectDT.itemsets <- function(x, precision = 3, ...) {
  df <- DATAFRAME(x)
  DT::datatable(df,
    filter = "top",
    rownames = paste0("[", rownames(df), "]"),
    ...
  ) %>%
    DT::formatRound(columns = which(sapply(df, is.numeric)), digits = max(precision))
}

#' @rdname inspectDT
#' @export
inspectDT.data.frame <- function(x, precision = 3, ...) {
  df <- x
  DT::datatable(df,
    filter = "top",
    rownames = paste0("[", rownames(df), "]"),
    ...
  ) %>%
    DT::formatRound(columns = which(sapply(df, is.numeric)), digits = max(precision))
}
