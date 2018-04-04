\name{shiny}
\Rdversion{1.1}
\alias{shiny_arules}
\title{Visualize Association Rules and Itemsets with Shiny}
\description{
Visualize assocation rules and itemsets using \pkg{shiny}.
}
\usage{
shiny_arules(x, support=0.1, confidence = 0.8)
}
\arguments{
  \item{x}{ a set of rules, a transactions object or a data.frame. } 
  \item{support, confidence}{initial minimum support and confidence to mine rules if
  transactions or a data.frame are supplied. Ignored for rules.}
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
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.8))
rules

shiny_arules(rules)

shiny_arules(Groceries, supp = 0.002, conf = 0.8)
}
}
