predict.values = function(base.function = "CAPP Historical 2012") {
  
  
  for (row.nr in which(Compilation.schemes$BelongsTo != "")) {
    scheme.names = strsplit(Compilation.schemes$BelongsTo[row.nr], "[;]")[[1]]
    for (Belongs.To in scheme.names) {
     
      Belongs.To = str_trim(Belongs.To, side = "both")
      index.row = intersect(which(correlation.data$Curve == Belongs.To), 
                            which(correlation.data$Base.Function == base.function))
      list.index = as.numeric(correlation.data$List.Number[index.row])
      lm.model = list.of.lm[[list.index]]
      
      Curve.to.resimulate = Compilation.schemes$Scheme[row.nr]
      new.dataframe = dataset[c("Dates", Curve.to.resimulate)]
      #browser()
      #new.dataframe = subset(new.dataframe,!is.na(new.dataframe[,2]))
      newdata = data.frame(x = new.dataframe[[2]])
      
      prediction = predict(lm.model, newdata = newdata, interval="predict", level = 0.98, na.action = na.pass)
      ### GIVES ERROR!
      dataframe = cbind(new.dataframe["Dates"], dataset[base.function], dataset[Belongs.To], 
                        dataset[Curve.to.resimulate] , prediction)
      n = length(names(dataframe))
      names(dataframe)[(n-2):n] = c("Most probable case", "Lower Bound", "Upper Bound")
      
      subtext = paste("On the basis of \"", Compilation.schemes$Short.Name[which(Compilation.schemes$Scheme == Belongs.To)],
                      "\" - Days evaluated: ", correlation.data$Time.Frame[index.row], collapse = "")
      setwd("plots")
      dir.create("prognosis", showWarnings = FALSE)
      setwd("prognosis")
      multi.line.plot(dataset = dataframe, main = give.shortname(Curve.to.resimulate), sub = subtext, shade = TRUE)
      setwd("../..")
    }
  }
}