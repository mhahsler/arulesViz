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

limit <- function(x, max, shading = NULL, measure = NULL, quiet = FALSE) {
  if (is.null(max)) return(x)
  if (length(x) <= max) return(x)
  
  l <- if (!is.null(shading)) shading[1] else measure[1]
  
  if (!quiet)
    warning(
      "Too many ", class(x), " supplied. Only plotting the best ",
      max, " using ",
      if (!is.null(l)) sQuote(l) else "the original order",
      " (change control parameter max if needed).",
      call. = FALSE
    )
  
  head(x,
    n = max,
    by = l,
    decreasing = TRUE)
}
