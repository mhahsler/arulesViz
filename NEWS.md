# arulesViz 1.3-3.1 (xx/xx/2019)


# arulesViz 1.3-3 (05/20/2019)

## New features
* ruleExplorer got a cleaner interface and several new features.

## Bug Fix
* plot with method "paracoord" works now also with a single item in the LHS. 

# arulesViz 1.3-2 (12/04/2018)

## Bug Fix
* ruleExplorer now checks for an excessive number of items so shiny does not freeze.
* plot with method matrix: control option reorder = "similarity" works now again.

# arulesViz 1.3-1 (04/23/2018)

## New features
* ruleExplorer: interactive visualization with shiny was added.
* plot method matrix now supports different reordering methods: 'none',
    'measure', 'support/confidence', 'similarity'. The default is measure,
    the measure used for shading.
* measure and shading now auto-completes measure names.

## Changes
* plotly_arules is deprecated use plot with engine = "plotly" instead

## Bug Fix
* Fixed problem with quality measures (offset of 1) when creating igraph 
    graphs (reported by sostahl). 

# arulesViz 1.3-0 (09/07/2017)

## New features
* Introduced engine parameter for plot which can be used to create htmlWidgets.
    Matrices and scatterplots use plotly and graphs are plotted 
    using visNetwork.

## Changes
* Some work was done on unifying the interface for plot more resulting in a
    slightly changed order of parameters.
* Parameter interactive is now deprecated and will be removed in future
    releases. Use engine = "interactive" or engine = "htmlWidget" 
    (where available).
* matrix-based visualization: reordering now tries to order by measure of 
  interestingness. The plot now uses shading instead of measure.
* method graph does not support type anymore. Only items are now supported as 
  vertices.

## Bug Fix
* Fixed item labels order in reordered parallel coordinates plot 
    (reported by Yvi24).


# arulesViz 1.2-1 (03/12/2017)

## New features

* plot method "grouped" now shows the most interesting items instead of the
  most frequent items to represent antecedents. Also, if more than 10 RHS
  items are found, then only the 10 most important are shown (see control 
  argument rhs_max).
* arules_plotly uses now signif instead of round for quality measures. 
* arules_plotly implements now jitter to reduce overplotting (defaults
    to .1 if overplotting would occur).
* arules_plotly shows correct rule indices for too large rule sets. 
* scatterplot also defaults jitter to .2 if overplotting would occur.
* scatterplot now handles Inf with a warning.

# arulesViz 1.2-0 (10/02/2016)

* improved graphical parameters for the grouped matrix plot.
* default color scheme is now a grey-red ramp.
* added inspectDT for interactive rules inspection using package DT.
* added plotly_arules, interactive plots with plotly.

# arulesViz 1.1-1 (04/10/2016)

* plot method ordered now passes control arguments on correctly.

# arulesViz 1.1-0 (12/13/2015)

* abbreviate has been moved to arules. 
* fixed bug in grouped plots when the quality of many rules is identical.
* plots use now heat colors. 
* scatter plots use now for lift zlim starting at 1.

# arulesViz 1.0-4 (09/15/2015)

* fixed empty LHS problem in grouped plot (reported by Andrew Collier)
* Updated for arules version 1.2.0

# arulesViz 1.0-2 (6/29/2015)

* fixed imports from non-base standard packages.
* plot (graph) now honors font family set via par(). 
      (Bug reported by May Yang) 
* igraph: Updated NAMESPACE since igraph introduced functions with 
      names that clash with arules and seriation. 
      Transitioned to igraphs new layout me# Chanism. 
* itemsets: scatterplot is now also defaut for itemsets.

# arulesViz 1.0-0 (12/07/2014)

* Improved LHS annotation for grouped plot

# arulesViz 0.1-9 (3/10/2014)

* Cleaned up dependencies on Rgraphviz

# arulesViz 0.1-8 (2/18/2014)

* Cleaned up dependencies (moved most to imports)

# arulesViz 0.1-7 (8/11/2013)

* plot checks now if 0 rules/itemsets are used
* scatterplots with a single rule now display correctly
* transition from igraph0 to igraph

# arulesViz 0.1-6 (5/25/2013)

* plot method matrix does not accept custom colors 

# arulesViz 0.1-5 (3/02/2012)

* fixed namespace (imports seriation now)

# arulesViz 0.1-4 (11/7/2011)

* fixed font family for igraph under windows (is now Arial).

# arulesViz 0.1-0 (12/17/2010)

* Initial version.
