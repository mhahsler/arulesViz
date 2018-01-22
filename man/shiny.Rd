\name{shiny}
\Rdversion{1.1}
\alias{shiny_arules}
\title{Visualize Association Rules and Itemsets with Shiny}
\description{
Visualize assocation rules and itemsets using \pkg{shiny}.
}
\usage{
shiny_arules(dataset, vars=5, supp=0.1, conf=0.5)
}
\arguments{
  \item{dataset}{Base data frame from which rules will be mined and visualized}
  \item{vars}{Default number of variables to include in initial rule mining}
  \item{supp}{Default minimum support threshold for rule mining}
  \item{conf}{Default minimum confidence threshold for rule mining}
}
%\details{}
%\value{}
\author{
Tyler Giallanza. Adapted from functions originally created by Andrew Brooks.
See http://brooksandrew.github.io/simpleblog/articles/association-rules-explore-app/
for more information and https://github.com/brooksandrew/Rsenal for the 
original code.
}
%\references{}
%\seealso{}
\examples{
data(iris)
\dontrun{
  shiny_arules(iris)
}
}
