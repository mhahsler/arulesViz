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


## color palettes
.col_picker <- function(level, palette, alpha = NULL) {
  col <- palette[floor(level * (length(palette) - 1)) + 1]
  if (!is.null(alpha)) {
    col <- apply(
      sapply(col, grDevices::col2rgb) / 255, 2,
      function(x) {
        grDevices::rgb(x[1], x[2], x[3], alpha = alpha)
      }
    )
  }
  col
}

### default are gray - > red
default_colors <- function(n, alpha = 1) {
  grDevices::colorRampPalette(c("#EE0000", "#EE9999", "#EEEEEE"), alpha = alpha)(n)
}
