
h_barplot <-
  function (x, x.labels, x.color)
  {

    length.labels <- round(max(nchar(x.labels))/2.9,0) + 5
    
    par(mar=c(2,max(8,length.labels/2.2),2,1), xpd = TRUE)
    
    xx <- barplot(height = x[length(x):1],
            names.arg = sapply(x.labels[length(x):1], 
                               function(s) paste(strwrap(s, length.labels), 
                                                 collapse = "\n"), 
                               USE.NAMES = FALSE),
            las = 1,
            legend.text = FALSE,
            horiz = TRUE,
            col = x.color, 
            xlab = "",
            width = 0.75,
            space = 0.5,
            border = NA,
            axes = FALSE,
            xlim = c(0,max(x)+10)
            )
    
    # Add horizontal axis:
    #axis(side = 1, lwd = 0)
    
    # Add values:
    
    if(qID != "Q11.5_rev"){
    
        text(y = xx, x = x[length(x):1], 
           label = paste(x[length(x):1], "%", sep =""), 
           pos = 4, cex = 0.8)
      
    } else {
        text(y = xx, x = x[length(x):1], 
             label = paste(x[length(x):1], sep =""), 
             pos = 4, cex = 0.8)
    }
    
}