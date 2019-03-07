
h_barplot_stacked <-
  function (x, x.labels, x.color, x.leged)
  {

    par(mar=c(5,12,4,3), xpd = TRUE)
    
    barplot(height = x,
            xlim = c(0,100),
            names.arg = sapply(x.labels, 
                               function(s) paste(strwrap(s, 28), 
                                                 collapse = "\n"), 
                               USE.NAMES = FALSE),
            las = 1,
            legend.text = FALSE,
            horiz = TRUE,
            col = x.color, 
            xlab = "percent",
            width = 0.75,
            space = 0.5,
            border = NA,
            axes = FALSE)
    
    # Add horizontal axis:
    axis(side = 1, lwd = 0)
    
    # Add legend to top right, outside plot region
    legend("topright", 
           horiz = TRUE,
           legend=x.legend,
           fill = x.color,
           bty = "n",
           inset=c(0.225,-.3))
    
    
}