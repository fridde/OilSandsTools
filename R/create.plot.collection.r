create.plot.collection = function() {
  
  schemes = names(Compilation.schemes)
  schemes = schemes[(which(schemes == "KeepEmpty")+1): length(schemes)]
  year.intervals = which(years.to.analize == "x", arr.ind = TRUE)
  
  for (ii in 1:length(year.intervals[,1])) {
    start.year = years.to.analize$from.to[year.intervals[ii,1]]
    end.year = gsub("X", "", names(years.to.analize)[year.intervals[ii,2]])
    directory.name = paste("plots/", start.year, "_", end.year, sep = "")
    cat(directory.name, " - ")
    dir.create(directory.name, showWarnings = FALSE)
    setwd(directory.name)
    current.dataset = time.filter(dataset, start.year, end.year)
    for (scheme in schemes) {
      #cat(scheme)
      #browser()
      filtered.dataset = filter.dataframe(current.dataset, scheme)
      multi.line.plot(filtered.dataset, scheme)
    }
    setwd("../..")
  }
}