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


# use library DT

inspectDT <- function(x, ...) UseMethod("inspectDT", x)

inspectDT.default <- function(x, ...) 
  stop("inspect 2 not implemented for class ", class(x))

inspectDT.rules <- function(x, precision = 2, ...) {
  datatable(
    data.frame(LHS = labels(lhs(x)), RHS = labels(rhs(x)), quality(x)), 
    filter = "top", 
    rownames = paste0('[', 1:length(x), ']'), ...) %>% formatRound(3:(3+ncol(quality(x))), 
        precision)
}

inspectDT.itemsets <- function(x, precision = 2, ...) {
  datatable(
    data.frame(items = labels(x), quality(x)), 
    filter = "top", rownames = paste0('[', 1:length(x), ']'), 
    ...) %>% formatRound(2:(2+ncol(quality(x))), 
      precision)
}
