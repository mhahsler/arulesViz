# Explore Association Rules Interactively

Explore association rules using interactive manipulations and
visualization using shiny.

## Usage

``` r
ruleExplorer(x, sidebarWidth = 2, graphHeight = "600px")
```

## Arguments

- x:

  a set of rules, a transactions object or a data.frame.

- sidebarWidth:

  width of the sidebar as a number between 0 (= 0% of the display width)
  and 12 (= 100% of the display width).

- graphHeight:

  height of the plots in pixels. Increase if you have a larger/higher
  resolution display.

## Value

returns a shiny app.

## References

Hahsler M (2017). arulesViz: Interactive Visualization of Association
Rules with R. *R Journal,* 9(2):163-175. ISSN 2073-4859.
[doi:10.32614/RJ-2017-047](https://doi.org/10.32614/RJ-2017-047) .

## See also

[`plot()`](http://michael.hahsler.net/arulesViz/reference/plot_arulesViz.md)
with `engine = "html"`,
[`inspectDT()`](http://michael.hahsler.net/arulesViz/reference/inspectDT.md),
[`arules::apriori()`](https://rdrr.io/pkg/arules/man/apriori.html).

## Author

Tyler Giallanza and Michael Hahsler. Adapted from functions originally
created by Andrew Brooks. See
[Rsenal](https://github.com/brooksandrew/Rsenal) for the original code.

## Examples

``` r
if (FALSE) {
# this example can only be run manually from the console.

# explore pre-mined rules 
data(Groceries)
rules <- apriori(Groceries, parameter = 
    list(support = 0.001, confidence = 0.8))

ruleExplorer(rules)

# mine and explore rules on the fly
ruleExplorer(iris)
}
```
