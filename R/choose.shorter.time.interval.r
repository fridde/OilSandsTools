choose.shorter.time.interval = function(data.table){
  
  # eliminate duplicates due to overwriting more exact data with less exact data
  
  Project = standardize.name(production.data$Project, type="Project")
  Company = standardize.name(production.data$Company, type="Company")
  Data.Source.Quality = rep.int(0, nrow(data.table))
  Data.Source.Quality[data.table$Data.Source == "http://environment.alberta.ca/"] = 1
  
  time.intervals = sort(as.integer(unique(data.table$Time.Interval)))
  pick.away = c()
  for (i.time.intervals in time.intervals) {
    right.time.interval = which(data.table$Time.Interval == paste(i.time.intervals))
    for (i.right.time.interval in right.time.interval) {
      same.project = which(Project == Project[i.right.time.interval])
      same.company = which(Company == Company[i.right.time.interval])
      
      larger.time.interval = which(as.integer(data.table$Time.Interval) > i.time.intervals )
      worse.data.quality = which(Data.Source.Quality < Data.Source.Quality[i.right.time.interval])
      
      same.date = which(data.table$Dates == data.table$Dates[i.right.time.interval])
      
      part1 = intersect(same.project, same.company)
      part2 = union(worse.data.quality, larger.time.interval)
      part2 = intersect(part2, same.date)
      pick.away = unique(c(pick.away, intersect(part1, part2)))
    }
  }
  return.data.table = data.table
  if (length(pick.away) > 0) {return.data.table = data.table[-pick.away,]}
 
  return (return.data.table)
  
}