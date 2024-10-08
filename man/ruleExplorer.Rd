% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ruleExplorer.R
\name{ruleExplorer}
\alias{ruleExplorer}
\alias{explore}
\title{Explore Association Rules Interactively}
\usage{
ruleExplorer(x, sidebarWidth = 2, graphHeight = "600px")
}
\arguments{
\item{x}{a set of rules, a transactions object or a data.frame.}

\item{sidebarWidth}{width of the sidebar as a number between 0 (= 0\% of the
display width) and 12 (= 100\% of the display width).}

\item{graphHeight}{height of the plots in pixels. Increase if you have a
larger/higher resolution display.}
}
\value{
returns a shiny app.
}
\description{
Explore association rules using interactive manipulations and visualization
using \pkg{shiny}.
}
\examples{
\dontshow{if (FALSE ) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# this example can only be run manually from the console.

# explore pre-mined rules 
data(Groceries)
rules <- apriori(Groceries, parameter = 
    list(support = 0.001, confidence = 0.8))

ruleExplorer(rules)

# mine and explore rules on the fly
ruleExplorer(iris)
\dontshow{\}) # examplesIf}
}
\references{
Hahsler M (2017). arulesViz: Interactive Visualization of
Association Rules with R. \emph{R Journal,} 9(2):163-175. ISSN 2073-4859.
\doi{10.32614/RJ-2017-047}.
}
\seealso{
\code{\link[=plot]{plot()}} with \code{engine = "html"}, \code{\link[=inspectDT]{inspectDT()}}, \code{\link[arules:apriori]{arules::apriori()}}.
}
\author{
Tyler Giallanza and Michael Hahsler.  Adapted from functions
originally created by Andrew Brooks. See
\href{https://github.com/brooksandrew/Rsenal}{Rsenal} for the original code.
}
