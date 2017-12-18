\name{shiny}
\Rdversion{1.1}
\alias{shiny_arules}
\title{Visualize Association Rules and Itemsets with Shiny}
\description{

}
\usage{
shiny_arules(dataset, bin=T, vars=5, supp=0.1, conf=0.5)
}
\arguments{
  \item{dataset}{Base data frame from which rules will be mined and visualized}
  \item{bin}{TODO}
  \item{vars}{TODO}
  \item{supp}{Default minimum support threshold for rule mining}
  \item{conf}{Default minimum confidence threshold for rule mining}
}
\details{

}
\value{

}
\author{
}
\references{
}

\seealso{

}
\examples{
data(iris)
\dontrun{
  shiny_arules(iris)
}
}
