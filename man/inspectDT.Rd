\name{inspectDT}
\Rdversion{1.1}
\alias{inspect}
\alias{inspectDT}
\alias{datatable}
\title{Inspect Associations Interactively using datatable}
\description{
Uses \pkg{datatable} to create a HTML table widget using the DataTables 
library. Rules can be interactively filtered and sorted.
}
\usage{
inspectDT(x, ...) 
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets". }
  \item{...}{ additional arguments. \code{precision} controls the precision
  used to print the quality measures (defaults to 2). All other arguments 
  are passed on to \code{datatable}.}
}
%\details{}
\value{
A datatable htmlwidget.
}
\author{
Michael Hahsler
}
%\references{
%}
\seealso{
\code{\link[DT]{datatable}} in \pkg{DT}.
}
\examples{
\dontrun{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
rules

inspectDT(rules)

### save table as a html page.
p <- inspectDT(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")}
}
\keyword{ print }
