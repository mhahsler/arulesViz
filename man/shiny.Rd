\name{shiny}
\Rdversion{1.1}
\alias{shiny_arules}
\title{Visualize Association Rules and Itemsets with Shiny}
\description{
Visualize assocation rules and itemsets using \pkg{shiny}.
}
\usage{
shiny_arules(x, parameter)
}
\arguments{
  \item{x}{ a set of rules, a transactions object or a data.frame. } 
  \item{parameter}{a list with parameters passed on to \code{\link[arules]{apriori}}. 
    the list can be used to set the initial support and confidence, etc.
    Values are ignored if \code{x} contains rules.}
}
%\details{}
%\value{}
\author{
Tyler Giallanza and Michael Hahsler. 
Adapted from functions originally created by Andrew Brooks.
See https://github.com/brooksandrew/Rsenal for the original code.
}
%\references{}
%\seealso{}
\examples{
\dontrun{
data(Groceries)

# visualize pre-mined rules
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

shiny_arules(rules)

# mine and visualize rules on the fly
shiny_arules(Groceries)
}
}
