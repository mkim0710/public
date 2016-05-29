### plot.pdf() ----
plot.pdf <- function( result, ... ) {
  pdf(sprintf("%s_plot.pdf", deparse(substitute(result)) 
  )
  )
  plot(result, ...)
  dev.off()
}