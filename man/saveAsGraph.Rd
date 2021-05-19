\name{associations2igraph}
\Rdversion{1.1}
\alias{saveAsGraph}
\alias{associations2igraph}
\title{Convert rules or itemsets into a graph}
\description{
Function to convert associations (rules, itemsets) into a igraph object and  
saves the graph in different formats (e.g., GraphML, dimacs, dot).
}
\usage{
associations2igraph(x)
saveAsGraph(x, file, format="graphml")
}
\arguments{
  \item{x}{ an object of class "rules" or "itemsets".}
  \item{file}{ file name.}
  \item{format}{ file format (e.g., "edgelist", 
  "graphml", "dimacs", "gml", "dot"). See \code{write.graph} in package 
  \pkg{igraph}.}
}
\value{
\code{associations2igraph} returns an igraph object.
}
\seealso{
\code{\link{plot}}, \code{\link[igraph]{write.graph}} in \pkg{igraph}
}
\author{Michael Hahsler}
\examples{
data("Groceries")
rules <- apriori(Groceries, parameter=list(support = 0.01, confidence = 0.5))

# convert rules into a graph
g <- associations2igraph(rules)
g

plot(g)

# save rules as a graph so they can be visualized using external tools
saveAsGraph(rules, "rules.graphml")

## clean up
unlink("rules.graphml")
}
\keyword{ file }
