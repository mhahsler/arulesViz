% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspectDT.R
\name{inspectDT}
\alias{inspectDT}
\alias{inspect}
\alias{datatable}
\alias{inspectDT.default}
\alias{inspectDT.rules}
\alias{inspectDT.itemsets}
\alias{inspectDT.data.frame}
\title{Inspect Associations Interactively Using datatable}
\usage{
inspectDT(x, ...)

\method{inspectDT}{default}(x, ...)

\method{inspectDT}{rules}(x, precision = 3, ...)

\method{inspectDT}{itemsets}(x, precision = 3, ...)

\method{inspectDT}{data.frame}(x, precision = 3, ...)
}
\arguments{
\item{x}{an object of class "rules" or "itemsets".}

\item{...}{additional arguments are passed on to \code{\link[DT:datatable]{DT::datatable()}}.}

\item{precision}{controls the precision used to print the quality measures (defaults to 2).}
}
\value{
A datatable htmlwidget.
}
\description{
Uses \pkg{datatable} to create a HTML table widget using the DataTables
library. Rules can be interactively filtered and sorted.
}
\references{
Hahsler M (2017). arulesViz: Interactive Visualization of
Association Rules with R. \emph{R Journal,} 9(2):163-175. ISSN 2073-4859.
\doi{10.32614/RJ-2017-047}.
}
\seealso{
\code{\link[DT:datatable]{DT::datatable()}} in \pkg{DT}.
}
\author{
Michael Hahsler
}
\keyword{print}
