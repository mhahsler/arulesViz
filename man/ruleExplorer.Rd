\name{ruleExplorer}
\Rdversion{1.1}
\alias{ruleExplorer}
\alias{explore}
\title{Explore Association Rules Interactively}
\description{
Explore association rules using interactive manipulations and visualization using \pkg{shiny}.
}
\usage{
ruleExplorer(x, sidebarWidth = 2, graphHeight = '600px')
}
\arguments{
  \item{x}{ a set of rules, a transactions object or a data.frame. } 
  \item{sidebarWidth}{ width of the sidebar as a number between 0 (= 0\% of the display width) and 
  12 (= 100\% of the display width). }
  \item{graphHeight}{ height of the plots in pixels. Increase if you have a larger/higher resolution display. }
}
\details{
}
%\value{}
\author{
Tyler Giallanza and Michael Hahsler. 
Adapted from functions originally created by Andrew Brooks.
See https://github.com/brooksandrew/Rsenal for the original code.
}
\references{
Hahsler M (2017). arulesViz: Interactive Visualization of Association
Rules with R. \emph{R Journal,} 9(2):163-175. ISSN 2073-4859. \doi{10.32614/RJ-2017-047}.
}
\seealso{
    \code{\link{plot}} with \code{engine = "html"},
    \code{\link{inspectDT}},
    \code{\link[arules]{apriori}}.
}
\examples{
\dontrun{
data(Groceries)

# explore pre-mined rules
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

ruleExplorer(rules)

# mine and explore rules on the fly
data(iris)
ruleExplorer(iris)
}
}
