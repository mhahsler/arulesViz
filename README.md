# arulesViz - Visualizing Association Rules and Frequent Itemsets - R package

[![CRAN version](http://www.r-pkg.org/badges/version/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arulesViz)](https://cran.r-project.org/package=arulesViz)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/arulesViz.svg?branch=master)](https://travis-ci.org/mhahsler/arulesViz)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/arulesViz?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/arulesViz)

This R package 
extends package [arules](http://github.com/mhahsler/arules) with various visualization techniques for association rules and itemsets. The package also includes several interactive visualizations for rule exploration.

## Installation

* __Stable CRAN version:__ install from within R.
* __Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/arulesViz/build/artifacts) or install via `install_git("mhahsler/arulesViz")` (requires devtools). You
might also have to install the development version of [arules](http://github.com/mhahsler/arules).

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

## Examples
```R
R> library(arulesViz)
R> data(Groceries)
R> rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
```

### Standard visualization
```R
R> plot(rules)
```

![Scatter plot](https://raw.githubusercontent.com/mhahsler/arulesViz/master/README/plot.png)

### Interactive visualization with plotly
```R
R> plotly_arules(rules)
```
[Click for Output](https://rawgit.com/mhahsler/arulesViz/master/README/plotly_arules.html)

### Interactive inspect with datatable
```R
R> inspectDT(rules)
```
[Click for Output](https://rawgit.com/mhahsler/arulesViz/master/README/inspectDT.html)




## Further Information
* List of changes from [NEWS.md](https://github.com/mhahsler/arulesViz/blob/master/NEWS.md)
* [arulesViz package vignette](https://cran.r-project.org/package=arulesViz/vignettes/arulesViz.pdf) with complete examples.
* [Reference manual](https://cran.r-project.org/package=arulesViz/arulesViz.pdf)

_Maintainer:_ [Michael Hahsler](http://michael.hahsler.net)

