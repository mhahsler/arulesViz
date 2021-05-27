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


# use library DT

inspectDT <- function(x, ...)
  UseMethod("inspectDT", x)

inspectDT.default <- function(x, ...)
  stop("inspect 2 not implemented for class ", class(x))

inspectDT.rules <- function(x, precision = 3, ...) {
  df <-  DATAFRAME(x)
  DT::datatable(df,
    filter = "top",
    rownames = paste0('[', rownames(df), ']'),
    ...) %>%
    DT::formatRound(columns =  which(sapply(df, is.numeric)), digits = max(precision))
}

inspectDT.itemsets <- function(x, precision = 3, ...) {
  df <-  DATAFRAME(x)
  DT::datatable(df,
    filter = "top",
    rownames = paste0('[', rownames(df), ']'),
    ...) %>%
    DT::formatRound(columns =  which(sapply(df, is.numeric)), digits = max(precision))
}

inspectDT.data.frame <- function(x, precision = 3, ...) {
  df <-  x
  DT::datatable(df,
    filter = "top",
    rownames = paste0('[', rownames(df), ']'),
    ...) %>%
    DT::formatRound(columns =  which(sapply(df, is.numeric)), digits = max(precision))
}
