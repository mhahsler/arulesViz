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

## Available Visualizations

* Scatterplot, two-key plot
* Matrix and matrix 3D visualization
* Grouped matrix-based visualization
* Several graph-based visualizations
* Doubledecker and mosaic plots
* Parallel Coodfinate plot

Several visualizations are also available in interactive exploration mode. 

## Example
```R
R> library(arulesViz)
R> data(Groceries)
R> rules <- apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
R> plot(rules)
```

## Further Information

* [arulesViz package vignette](https://cran.r-project.org/package=arulesViz/vignettes/arulesViz.pdf) with complete examples.
* [Reference manual](https://cran.r-project.org/package=arulesViz/arulesViz.pdf)

_Maintainer:_ [Michael Hahsler](http://michael.hahsler.net)

