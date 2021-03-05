# arulesViz - Visualizing Association Rules and Frequent Itemsets with R

[![CRAN version](https://www.r-pkg.org/badges/version/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![Rdoc](https://www.rdocumentation.org/badges/version/arulesViz)](https://www.rdocumentation.org/packages/arulesViz) 
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![R build status](https://github.com/mhahsler/arulesViz/workflows/R-CMD-check/badge.svg)](https://github.com/mhahsler/arulesViz/actions)

This R package 
extends package [arules](https://github.com/mhahsler/arules) with various visualization techniques for association rules and itemsets. The package also includes several interactive visualizations for rule exploration.

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("arulesViz")
```
__Current development version:__ install from GitHub (needs devtools) with
```R 
devtools::install_github("mhahsler/arulesViz")
```
This might also require the development version of [arules](https://github.com/mhahsler/arules).

## Features
* Visualizations using `base`, `grid`, `ggplot2`, `plotly`, and `viznetwork`. 
* Interactive visualizations using `grid` and `plotly`.
* Interactive rule inspection with `datatable`.
* Integrated interactive rule exploration using `ruleExplorer`. 

### Available Visualizations:

* Scatterplot, two-key plot
* Matrix and matrix 3D visualization
* Grouped matrix-based visualization
* Several graph-based visualizations
* Doubledecker and mosaic plots
* Parallel Coordinate plot

## Usage 

Mine some rules.
```R
library(arulesViz)
data(Groceries)
rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
```

### Standard visualization
```R
plot(rules)
```

![Scatter plot](https://raw.githubusercontent.com/mhahsler/arulesViz/master/README/plot.png)

### Interactive visualization with plotly
```R
plot(rules, engine = "plotly")
```

[Open example output.](https://mhahsler.github.io/arulesViz/README/plotly_arules.html)


### Interactive inspect with datatable
```R
inspectDT(rules)
```

[Open example output.](https://mhahsler.github.io/arulesViz/README/inspectDT.html)

## References

* Michael Hahsler and Sudheer Chelluboina. [Visualizing Association Rules: Introduction 
to the R-extension Package arulesViz](https://cran.r-project.org/package=arulesViz/vignettes/arulesViz.pdf) (with complete examples). 
* Michael Hahsler. [arulesViz: Interactive visualization of association rules with R.](https://journal.r-project.org/archive/2017/RJ-2017-047/RJ-2017-047.pdf) _R Journal,_ 9(2):163-175, December 2017.
* Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian Buchta. [The arules R-package ecosystem: Analyzing interesting patterns from large transaction datasets.](https://jmlr.csail.mit.edu/papers/v12/hahsler11a.html) _Journal of Machine Learning Research,_ 12:1977-1981, 2011.


