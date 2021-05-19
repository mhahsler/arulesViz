\name{inspectDT}
\Rdversion{1.1}
\alias{inspect}
\alias{inspectDT}
\alias{datatable}
\title{Inspect Associations Interactively Using datatable}
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
  are passed on to \code{datatable} in package \pkg{DT}.}
}
%\details{}
\value{
A datatable htmlwidget.
}
\author{
Michael Hahsler
}
\references{
Hahsler M (2017). arulesViz: Interactive Visualization of Association
Rules with R. \emph{R Journal,} 9(2):163-175. ISSN 2073-4859. \doi{10.32614/RJ-2017-047}.
}
\seealso{
\code{\link[DT]{datatable}} in \pkg{DT}.
}
\examples{
\dontrun{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
rules

inspectDT(rules)

# for more control on the data table, you can used DATAFRAME() to convert the rules.
rules_df <- DATAFRAME(rules, setStart = '', setEnd = '', itemSep = ' + ')
rules_df$count <- NULL
head(rules_df)
inspectDT(rules_df)

# save a datatable as a html page.
p <- inspectDT(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained = FALSE)
browseURL("arules.html")}
}
\keyword{ print }
