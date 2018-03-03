\name{shiny}
\Rdversion{1.1}
\alias{shiny_arules}
\title{Visualize Association Rules and Itemsets with Shiny}
\description{
Visualize assocation rules and itemsets using \pkg{shiny}.
}
\usage{
shiny_arules(dataset, vars=0, supp=0.1, conf=0.5, lift = 0)
}
\arguments{
  \item{dataset}{Base transactions object from which rules will be mined and visualized. Also takes a rules object containing already mined rules.}
  \item{vars}{Default number of variables to include in initial rule mining. If a value is not passed to this parameter, all available variables will be used.}
  \item{supp}{Default minimum support threshold for rule mining}
  \item{conf}{Default minimum confidence threshold for rule mining}
  \item{lift}{Default minimum lift threshold for displayed rules}
}
%\details{}
%\value{}
\author{
Tyler Giallanza. Adapted from functions originally created by Andrew Brooks.
See https://github.com/brooksandrew/Rsenal for the original code.
}
%\references{}
%\seealso{}
\examples{
data(Adult)
\dontrun{
  shiny_arules(Adult)
}
}
