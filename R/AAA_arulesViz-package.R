#' @keywords internal
#'
#' @section Main Functions:
#'
#' * [`arulesViz::plot()`] plot association rules.
#' * [`inspectDT()`] for interactive rule tables.
#' * [`ruleExplorer()`] for interactive rule inspection with tables and graphs.
#'
#' @import arules
#' @import grid
#' @import ggplot2
#' @import arules
#' @import tibble
#' @import tidyr

# NOTE: dplyr has many NAMESPACE conflicts with arules so we use ::
# import("dplyr")

#' @importFrom graphics par mtext text axis box
#' @importFrom stats median
#' @importFrom methods is as new

# Otherwise we get:  object 'guide_edge_colourbar' of mode 'function' was not found
# Note: has also an alias in man page plot

#' @importFrom ggraph guide_edge_colourbar
#' @export guide_edge_colourbar
"_PACKAGE"
