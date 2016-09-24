\name{inspect2}
\Rdversion{1.1}
\alias{inspect}
\alias{inspect2}
\alias{datatable}
\title{Inspect Associations Interactively using datatable}
\description{
Uses \pkg{datatable} to create a HTML table widget using the DataTables 
library. Rules can be interactively filtered and sorted.
}
\usage{
inspect2(x, ...) 
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets". }
  \item{...}{ additional arguments. \code{precision} controls the precision
  used to print the quality measures (defaults to 2). All other arguments 
  are passed on to \code{datatable}.}
}
%\details{
%}
%\value{
%}
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

inspect2(rules)}
}
\keyword{ print }
