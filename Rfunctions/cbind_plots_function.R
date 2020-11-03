
cbind_plots <- function(...){
  # Put separate figures into one plot object, next to each other (cbind-style)
  suppressPackageStartupMessages(library(gtable))
  
#   if (! any(lapply(..., function(chk) (!any(class(chk)) == "gtable"))))
#     stop("cbind_max says: all input figures have to be of class() == 'gtable'!")
  
  gtl <- lapply(list(...), ggplotGrob) # expecting input to be already of class() == 'gtable'!
  
  bind2 <- function (x, y)
  {
    stopifnot(nrow(x) == nrow(y))
    if (ncol(x) == 0) 
      return(y)
    if (ncol(y) == 0) 
      return(x)
    y$layout$l <- y$layout$l + ncol(x)
    y$layout$r <- y$layout$r + ncol(x)
    x$layout <- rbind(x$layout, y$layout)
    x$widths <- gtable:::insert.unit(x$widths, y$widths)
    x$colnames <- c(x$colnames, y$colnames) # leave 'rownames'?
    x$heights <- grid::unit.pmax(x$heights, y$heights)
    x$grobs <- append(x$grobs, y$grobs)
    x
  }
  
  Reduce(bind2, gtl)
}

