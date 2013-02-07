give.shortname = function(long.name) {
  
  short.name = long.name
  for (i.long.name in 1:length(long.name)) {
    right.row = which(Compilation.schemes$Scheme == long.name[i.long.name])
    if (length(right.row) == 1) {
      short.name[i.long.name] = Compilation.schemes$Short.Name[right.row]
    }
  }
  return(short.name)
}