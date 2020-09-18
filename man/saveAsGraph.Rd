\name{saveAsGraph}
\Rdversion{1.1}
\alias{saveAsGraph}
\title{Save rules or itemsets as a graph description}
\description{
This function 
saves a rule sat as a graph description in different formats 
(e.g., GraphML, dimacs, dot).
}
\usage{
saveAsGraph(x, file, type="items", format="graphml")
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets".}
  \item{file}{ file name}
  \item{type}{ see type in plot with method "graph" 
  (e.g., "itemsets", "items").}
  \item{format}{ file format (e.g., "edgelist", 
  "graphml", "dimacs", "gml", "dot"). See \code{write.graph} in package 
  \pkg{igraph}.}
}
                            
%\details{}

\seealso{
\code{\link{plot}}, \code{\link[igraph]{write.graph}} in \pkg{igraph}
}
\author{Michael Hahsler}
\examples{
data("Groceries")
rules <- apriori(Groceries, parameter=list(support=0.01, confidence=0.5))

saveAsGraph(rules, "rules.graphml")

## clean up
unlink("rules.graphml")
}
\keyword{ file }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
