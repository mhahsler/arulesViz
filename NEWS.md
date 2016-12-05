# Changes in version 1.2-0.1 (xx/xx/2016)

* arules_plotly uses now signif instead of round for quality measures. 
* arules_plotly shows correct rule indices for too large rule sets. 
* arules_plotly implements now jitter to reduce overplotting (defaults
    to .1 if overplotting would occur)
* regular scatter plot also defaults jitter to .2 if overplotting would occur.
* plot method "grouped" now shows the most interesting items instead of the
  most frequent items to represent antecedents. Also, if more than 10 RHS
  items are found, then only the 10 most important are shown (see control 
  argument rhs_max)
* scatterplot now handles Inf with a warning.

# Changes in version 1.2-0 (10/02/2016)

* improved graphical parameters for the grouped matrix plot.
* default color scheme is now a grey-red ramp.
* added inspectDT for interactive rules inspection using package DT.
* added plotly_arules, interactive plots with plotly.

# Changes in version 1.1-1 (04/10/2016)

* plot method ordered now passes control arguments on correctly.

# Changes in version 1.1-0 (12/13/2015)

* abbreviate has been moved to arules. 
* fixed bug in grouped plots when the quality of many rules is identical.
* plots use now heat colors. 
* scatter plots use now for lift zlim starting at 1.

# Changes in version 1.0-4 (09/15/2015)

* fixed empty LHS problem in grouped plot (reported by Andrew Collier)
* Updated for arules version 1.2.0

# Changes in version 1.0-2 (6/29/2015)

* fixed imports from non-base standard packages.
* plot (graph) now honors font family set via par(). 
      (Bug reported by May Yang) 
* igraph: Updated NAMESPACE since igraph introduced functions with 
      names that clash with arules and seriation. 
      Transitioned to igraphs new layout me# Chanism. 
* itemsets: scatterplot is now also defaut for itemsets.

# Changes in version 1.0-0 (12/07/2014)

* Improved LHS annotation for grouped plot

# Changes in version 0.1-9 (3/10/2014)

* Cleaned up dependencies on Rgraphviz

# Changes in version 0.1-8 (2/18/2014)

* Cleaned up dependencies (moved most to imports)

# Changes in version 0.1-7 (8/11/2013)

* plot checks now if 0 rules/itemsets are used
* scatterplots with a single rule now display correctly
* transition from igraph0 to igraph

# Changes in version 0.1-6 (5/25/2013)

* plot method matrix does not accept custom colors 

# Changes in version 0.1-5 (3/02/2012)

* fixed namespace (imports seriation now)

# Changes in version 0.1-4 (11/7/2011)

* fixed font family for igraph under windows (is now Arial).

# Initial version 0.1-0 (12/17/2010)
