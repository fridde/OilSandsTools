multi.line.plot = function(dataset, main = NA, sub = NA, curves = NA, shade = FALSE) {

  if(is.na(curves)) {
    curves = names(dataset)[names(dataset) != "Dates"]
  }
  y.values = dataset[curves]
  x.vector = dataset$Dates
  
  #col = rainbow(length(curves))
  #col = terrain.colors(length(curves))
  col = rich.colors(length(curves))
  lty = 1:6
  lwd = 2
  pch.values = c(0:25, 32:127)
  
  
  
  xlim = as.numeric(c(dataset$Dates[1], dataset$Dates[length(dataset$Dates)]))
  if (!all(is.na(y.values))) {
    ylim = c(0, max(y.values, na.rm = TRUE))
    
    xlab = "Year"
    ylab = "Barrels per day"
    if(is.na(main)) {
      main =  "Production of Bitumen"
    } else {
      main = paste(strsplit(main, "\\.")[[1]], collapse = " ")
    }
    if(is.na(sub)) {
      sub = "Production from Oil Sands"
    }
    
    filename = paste(main, ".pdf",  sep = "")
    filename = gsub(" ","_", filename)
    pdf(file = filename)
    
    for (ii in 1:ncol(y.values)) {
      if(ii == 1) {
        plot(x.vector, y.values[, ii], xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = main, sub = sub,
             lty = lty[ii %% 6], col = col[ii], lwd = lwd, type = "l")
        if (shade) {
          x = c(dataset[["Dates"]] , rev(dataset[["Dates"]]))
          y = c(dataset[["Lower Bound"]], rev(dataset[["Upper Bound"]]))
          dataframe = subset(data.frame(x,y), !is.na(y))
          x = dataframe[,1]
          y = dataframe[,2]
          polygon(x,y, density = 10)
        }
      } else {
        lines(x.vector, y.values[, ii], col = col[ii], lty = lty[ii %% 6] , lwd = lwd)
        values.to.choose = seq(from = 1, to = length(x.vector), by = ceiling(length(x.vector) / 5 ))
        points(x.vector[values.to.choose] , 
               y.values[values.to.choose, ii] , pch = pch.values[ii-1], col = col[ii])
      }
    }
    legend("topleft", give.shortname(curves), col = col , lty = lty, lwd = lwd, cex = 0.7, pt.cex = 0.8, pt.lwd = 0.8 * lwd, 
           pch = pch.values[c(NA, 1:(length(curves)))])
    
    dev.off()
  }
}