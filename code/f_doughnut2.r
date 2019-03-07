
doughnut2 <-
  function (x, x.labels, x.color, x.legend)
  {

    x.labels <- paste(x.labels, " (", x, "%)", sep = "")
    
    length.labels <- round(max(nchar(x.labels))/3.5,0) + 10
    
    x.labels <- sapply(x.labels, 
                         function(s) paste(strwrap(s, length.labels), 
                                           collapse = "\n"), 
                         USE.NAMES = FALSE)
    
    
    print.labels <- x.labels
    print.labels[x<4] <- ""
    
    legend.labels <- x.labels[x<4]
    legend.colors <- x.color[x<4]
    
    
    if(sum(x<4)>0){
    
      par(mar=c(0,2,0,0), xpd = TRUE)
      
      
    } else {
      
      par(mar=c(0,0,0,0), xpd = FALSE)
      
    }
    
    doughnut(x ,
             labels = print.labels, 
             edges = 1000, 
             outer.radius = 0.70, 
             inner.radius=0.35, 
             col = x.color, 
             border = FALSE, 
             lty = NULL, 
             main = NULL,
             cex = 1)
    
    if(sum(x<4)>0){
      
      
      # Add legend to top right, outside plot region
      legend("bottom", 
             horiz = TRUE,
             legend=legend.labels,
             fill = legend.colors,
             bty = "n",
             inset=c(0,0))
      
     
    }

}