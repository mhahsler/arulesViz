#' @title `r packageDescription("arulesViz")$Package`: `r packageDescription("arulesViz")$Title`
#'
#' @description `r sub("<[^>]+>", "", packageDescription("arulesViz")$Description)`
#'
#' @section Main Functions:
#' 
#' * [`arulesViz::plot()`] plot association rules.
#' * [`inspectDT()`] for interactive rule tables.
#' * [`ruleExplorer()`] for interactive rule inspection with tables and graphs.
#' 
#' @author Michael Hahsler
#'
#' @references
#' Hahsler M (2017). arulesViz: Interactive Visualization of Association Rules with R. R Journal, 9(2), 163–175. ISSN 2073-4859, \doi{10.32614/RJ-2017-047}.
#'
#' @docType package
#' @name arulesViz-package
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
NULL
