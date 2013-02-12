\name{predict.values}
\alias{predict.values}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.values(base.function = "CAPP Historical 2012")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{base.function}{
%%     ~~Describe \code{base.function} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (base.function = "CAPP Historical 2012") 
{
    for (row.nr in which(Compilation.schemes$BelongsTo != "")) {
        scheme.names = strsplit(Compilation.schemes$BelongsTo[row.nr], 
            "[;]")[[1]]
        for (Belongs.To in scheme.names) {
            Belongs.To = str_trim(Belongs.To, side = "both")
            index.row = intersect(which(correlation.data$Curve == 
                Belongs.To), which(correlation.data$Base.Function == 
                base.function))
            list.index = as.numeric(correlation.data$List.Number[index.row])
            lm.model = list.of.lm[[list.index]]
            Curve.to.resimulate = Compilation.schemes$Scheme[row.nr]
            new.dataframe = dataset[c("Dates", Curve.to.resimulate)]
            newdata = data.frame(x = new.dataframe[[2]])
            prediction = predict(lm.model, newdata = newdata, 
                interval = "predict", level = 0.98, na.action = na.pass)
            dataframe = cbind(new.dataframe["Dates"], dataset[base.function], 
                dataset[Belongs.To], dataset[Curve.to.resimulate], 
                prediction)
            n = length(names(dataframe))
            names(dataframe)[(n - 2):n] = c("Most probable case", 
                "Lower Bound", "Upper Bound")
            subtext = paste("On the basis of \"", Compilation.schemes$Short.Name[which(Compilation.schemes$Scheme == 
                Belongs.To)], "\" - Days evaluated: ", correlation.data$Time.Frame[index.row], 
                collapse = "")
            setwd("plots")
            dir.create("prognosis", showWarnings = FALSE)
            setwd("prognosis")
            multi.line.plot(dataset = dataframe, main = give.shortname(Curve.to.resimulate), 
                sub = subtext, shade = TRUE)
            setwd("../..")
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
