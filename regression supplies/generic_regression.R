# Generic regressions script

data_independent <- fread(file.path(wd,"Datasources/file.csv"),stringsAsFactors=TRUE) # change to csv/rds name
data_dependent <- fread(file.path(wd,"Datasources/file.csv"),stringsAsFactors=TRUE) # change to csv/rds name

## if your data is not named properly, you may need to rename something to geoID:
# setnames(data, "oldname", "geoID")

dataset <- merge(data_independent, data_dependent, by="geoID", all.x=TRUE)

y <- datatset$dependent # change to variable name
x <- dataset$independent # change to variable name

do_stuff <- function(x,y){
  plot(x,y)
  model <- lm(y ~ x)
  return(paste("slope =",round(coef(model)["x"],digits=4),
        "; r squared =",round(summary(model)$r.squared,digits=4)))
  }

do_stuff(x,y)
