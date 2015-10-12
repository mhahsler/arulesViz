#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


doubledecker_arules <- function(rules, measure ="support", data, 
	control=list(), ...) {
    
    if(length(rules) != 1) stop("only can visualize one rule.")
    if(is.null(data)) stop("Data missing.")
    
    control <- .get_parameters(list(
		    main = "Doubledecker plot for 1 rule",
		    type = "doubledecker",
		    interactive = FALSE
		    ), control)

    table <- getTable(rules, data)

    if(control$type=="doubledecker")
    doubledecker(table, margins=c(2,8,length(dim(table) + 2), 2), 
	    main = control$main, ...) 
    else {
    control$main <- "Mosaic plot for 1 rule"
    mosaic(table, highlighting = length(dim(table)),
            main = control$main, ...)
    }
}


