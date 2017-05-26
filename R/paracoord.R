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


paracoord_arules <- function(x, measure= "support", shading = "lift", 
  control=list(), ...) {
  
  control <- .get_parameters(control, list(
    main = paste("Parallel coordinates plot for", length(x), "rules"),
    reorder = FALSE,
    interactive = FALSE,
    gp_labels = gpar(),
    newpage = TRUE,
    col = default_colors(100),
    alpha = NULL,
    quality = 2,
    verbose = FALSE
  ))
  
  
  ## remove short rules
  x <- x[size(x)>1]
  if(length(x)<1) stop("No rules of length 2 or longer.")
  
  
  ## sort rules to minimize occlusion
  x <- sort(x, by=shading,  decreasing = FALSE)
  lwd <- map(quality(x)[[measure]], c(1,5))
  col <- .col_picker(map(quality(x)[[shading]]), rev(control$col), 
    alpha = control$alpha)
   
  l <- LIST(lhs(x))
  r <- LIST(rhs(x))
  u <- union(unlist(l), unlist(r))
  n <- length(u)
  maxLenLHS <- max(sapply(l, length))
  
  pl <- t(sapply(l, FUN = function(x)  {
    x <- match(x, u)
    # reordering items of antecedent
    length(x) <- maxLenLHS
    rev(x) ## so NAs are to the left (we could also use na.last for sort)
  }))
  
  
  ## RHS is always a single item for now
  pr <- sapply(r, FUN = function(x)  match(x, u))
  
  m <- cbind(pl,pr)
  colnames(m) <- c(ncol(pl):1, "rhs")
  
  ### reduce crossovers
  if(control$reorder && length(x)>1) {
    count <- countCrossovers(m)
    noswapcount <- 0
    order <- seq(n)
    
    while(noswapcount < control$quality*n) {
      
      if(control$verbose) {
        cat("Current best count...", count, 
          "(no swaps for",noswapcount,"/",control$quality*n,"tries)\n")
      }
      
      ### try a random swap	    
      ij <- sample(n,2)
      i <- ij[1]
      j <- ij[2]
      
      order_tmp <- order
      order_tmp[j] <- order[i]
      order_tmp[i] <- order[j]
      
      pl_tmp <- matrix(order_tmp[pl], nrow=nrow(pl))
      pr_tmp <- order_tmp[pr]
      
      count_tmp <- countCrossovers(cbind(pl_tmp, pr_tmp))
      
      if(count_tmp < count) {
        noswapcount <- 0
        order <- order_tmp
        count <- count_tmp
      }else{
        noswapcount <- noswapcount+1
      }
    }
    
    pl[] <- order[pl]
    pr <- order[pr]
    u <- u[order(order)]
    
    m <- cbind(pl,pr)
    colnames(m) <- c(ncol(pl):1, "rhs")
  }
  
  
  ## start plot
  if(control$newpage) grid.newpage()
  
  ## main
  gTitle(control$main)
  
  ## plot
  leftSpace <- max(stringWidth(u))
  pushViewport(viewport(x=unit(2,"lines")+leftSpace, y=unit(4,"lines"),
    just = c("left","bottom"),
    width = unit(1, "npc")-unit(4,"lines")-leftSpace,
    height = unit(1, "npc")-unit(4+4,"lines"),
    default.units = "native", gp=control$gp_labels,
    name="paracoord"))
  
  gParacoords(m, xlab="Position", discreteNames = u, 
    col=col, lwd=lwd, arrowPos =ncol(m), 
    gp_lines=gpar(alpha=control$alpha))
  
}



paracoord_items <- function(x, measure= "support", shading = NULL,
  control=list(), ...) {
  
  control <- .get_parameters(control, list(
    main =paste("Parallel coordinates plot for", 
      length(x), "itemsets"),
    reorder = FALSE,
    interactive = FALSE,
    gp_labels = gpar(),
    newpage = TRUE,
    alpha = NULL
  ))
  
  
  ## remove single items
  x <- x[size(x)>1]
  
  ## sort to minimize occlusion
  x <- sort(x, by=measure,  decreasing = FALSE)
  lwd <- map(quality(x)[[measure]], c(1,5))
  #col <- gray(map(quality(x)[[shading]], c(0.8,0.1)))
  col <- NULL 
  
  i <- LIST(items(x))
  u <- unique(unlist(i))
  
  ## reorder
  ## maybe we can do better here (reorder items and positions)
  
  maxLen <- max(size(x))
  m <- t(sapply(i, FUN = function(x)  {
    x <- match(x, u)
    if(control$reorder) x <- sort(x, decreasing = TRUE)
    length(x) <- maxLen
    x 
  }))
  
  colnames(m) <- c(1:ncol(m))
  
  ## start plot
  if(control$newpage) grid.newpage()
  
  ## main
  gTitle(control$main)
  
  ## plot
  leftSpace <- max(stringWidth(u))
  pushViewport(viewport(x=unit(2,"lines")+leftSpace, y=unit(4,"lines"),
    just = c("left","bottom"),
    width = unit(1, "npc")-unit(4,"lines")-leftSpace,
    height = unit(1, "npc")-unit(4+4,"lines"),
    default.units = "native", gp=control$gp_labels,
    name="paracoord"))
  
  gParacoords(m, xlab="Position", discreteNames = u, 
    col=col, lwd=lwd,
    gp_lines=gpar(alpha=control$alpha))
  
}


#no use of this funtion can be deleted later
makeMatrix <- function(l=NULL, r=NULL, u=NULL, control=NULL)
{
  maxLenLHS <- max(sapply(l, length))
  pl <- t(sapply(l, FUN = function(x)  {
    x <- match(x, u)
    if(control$reorder) x <- sort(x, decreasing = TRUE)
    length(x) <- maxLenLHS
    rev(x) ## so NAs are to the left (we could also use na.last for sort)
  }))
  
  ## RHS is always 1 for now
  pr <- sapply(r, FUN = function(x)  match(x, u))
  
  m <- cbind(pl, pr)
  colnames(m) <- c(ncol(pl):1, "rhs")
  m
}

swap <- function(v=NULL, i=NULL, j=NULL)
{
  temp <- v[i]
  v[i] <- v[j]
  v[j] <- temp
  v
}

countCrossovers <- function(m=NULL)
{
  count <- 0
  for(i in 1:(ncol(m)-1))
  {
    for(j in 2:nrow(m))
    {
      if(!is.na(m[j,i]))
      {
        x <- m[j,i]
        y <- m[j,i+1]
        o <- which(m[1:j-1,i+1] > y)
        #print(o)
        # p <- which(m[1:j-1,i] < x)
        #print(p)
        l <- which(m[1:j-1,i] > x)
        #print(l)
        if(as.integer(length(o)) != 0)
        {
          for(k in 1:length(o))
          {
            #print(paste(o[k],i, sep=","))
            if(!is.na(m[o[k],i]))
              if(m[o[k],i] < x)
              {
                #	    print(paste(o[k],i, sep=","))
                count <- count+1
              }
          }
        }
        if(as.integer(length(l)) != 0)
        {
          for(k in 1:length(l))
          {
            if(!is.na(m[l[k],i+1]))
              if(m[l[k],i+1] < y)
                count <- count+1
          }
        }
      }
    }
  }
  count
}
