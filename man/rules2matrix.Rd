\name{rules2matrix}
\alias{rules2matrix}
\alias{rules2groupedMatrix}
\title{Convert association rules into a matrix}
\description{
Converts a set of association rules into a matrix with unique LHS itemsets as columns and unique RHS itemsets as rows. The matrix cells contain a quality measure. The LHS itemsets can be grouped.
}
\usage{
rules2matrix(rules, measure = "support", reorder = "measure", ...)
rules2groupedMatrix(rules, measure = "lift", measure2 = "support", k = 10, 
  aggr.fun = mean, lhs_label_items = 2)
}
\arguments{
  \item{rules}{ a rules object. }
  \item{measure}{ quality measure put in the matrix }
  \item{reorder}{ reorder rows and columns? Possible methods are: "none", "measure" (default), "support/confidence", "similarity". }
  \item{measure2}{ second quality measure (organized in the same way as measure). }
  \item{k}{ number of LHS itemset groups. }
  \item{aggr.fun}{ function to aggregate the quality measure for groups. }
  \item{lhs_label_items}{ number of top items used to name LHS itemset groups (columns). }
  \item{...}{ passed on to \code{\link{DATAFRAME}}. }
}
%\details{
%}
\value{
\code{rules2matrix} returns a matrix with quality values.

\code{rules2groupedMatrix} returns a list with elements
  \item{m}{ the grouped matrix for measure. }
  \item{m2}{ the grouped matrix for measure2. }
  \item{clustering_rules}{ vector with group assignment for each rule. }
}
\references{
Michael Hahsler and Radoslaw Karpienko. Visualizing association rules in hierarchical groups. Journal of Business Economics, 87(3):317--335, May 2016. \doi{10.1007/s11573-016-0822-8}.
}
\author{
Michael Hahsler
}
\seealso{
\code{\link{plot}} for rules using  \code{method = 'matrix'} and  \code{method = 'grouped matrix'}.
}
\examples{
data(Groceries)
rules <- apriori(Groceries, parameter=list(support = 0.001, confidence = 0.8))
rules

## Matrix
m <- rules2matrix(rules[1:10], measure = "lift")
m
plot(rules[1:10], method = "matrix")

## Grouped matrix
# create a matrix with LHSs grouped in k = 10 groups
m <- rules2groupedMatrix(rules, k = 10)
m$m

# number of rules per group 
table(m$clustering_rules)

# get rules for group 1
inspect(rules[m$clustering_rules == 1])

# the corresponding plot
plot(rules, method = "grouped matrix", k = 10)
}
