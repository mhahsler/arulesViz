# arulesViz - Visualizing Association Rules and Frequent Itemsets with R

[![CRAN version](http://www.r-pkg.org/badges/version/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![Rdoc](http://www.rdocumentation.org/badges/version/arulesViz)](http://www.rdocumentation.org/packages/arulesViz) 
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/arulesViz.svg?branch=master)](https://travis-ci.org/mhahsler/arulesViz)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/arulesViz?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/arulesViz)

This R package 
extends package [arules](https://github.com/mhahsler/arules) with various visualization techniques for association rules and itemsets. The package also includes several interactive visualizations for rule exploration.

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("arulesViz")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/arulesViz/build/artifacts) or install from GitHub (needs devtools).
```R 
library("devtools")
install_github("mhahsler/arulesViz")
```
This might also require the development version of [arules](http://github.com/mhahsler/arules).

## Features
* Visualizations using `base` and/or `grid`. 
* Interactive visualizations using `grid`.
* Interactive visualizations with package `plotly`.
* Interactive rule inspection with package `datatable`.

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
plotly_arules(rules)
```

<iframe src="https://rawgit.com/mhahsler/arulesViz/master/README/plotly_arules.html" width="100%" height="500">

[Open example output.](https://rawgit.com/mhahsler/arulesViz/master/README/plotly_arules.html)


### Interactive inspect with datatable
```R
inspectDT(rules)
```

<iframe src="https://rawgit.com/mhahsler/arulesViz/master/README/inspectDT.html" width="100%" height="800">
</iframe>

[Open example output.](https://rawgit.com/mhahsler/arulesViz/master/README/inspectDT.html)

## References

* [Intro article](https://cran.r-project.org/package=arulesViz/vignettes/arulesViz.pdf) with complete examples by Michael Hahsler and Sudheer Chelluboina.
* Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian Buchta. [The arules R-package ecosystem: Analyzing interesting patterns from large transaction datasets.](http://jmlr.csail.mit.edu/papers/v12/hahsler11a.html) _Journal of Machine Learning Research,_ 12:1977-1981, 2011.


