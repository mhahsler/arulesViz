\name{ruleExplorer}
\Rdversion{1.1}
\alias{ruleExplorer}
\alias{explore}
\title{Explore Association Rules Interactively}
\description{
Explore association rules using interactive manipulations and visualization using \pkg{shiny}.
}
\usage{
ruleExplorer(x, parameter = NULL)
}
\arguments{
  \item{x}{ a set of rules, a transactions object or a data.frame. } 
  \item{parameter}{a list with parameters passed on to \code{\link[arules]{apriori}}. 
    the list can be used to set the initial support and confidence thresholds.
    Values are ignored if \code{x} contains a set of rules.}
}
%\details{}
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
ruleExplorer(Groceries)
}
}
