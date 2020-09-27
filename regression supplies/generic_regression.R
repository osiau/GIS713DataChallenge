# Generic regressions script
# Toggle options for state vs. county (county is default)

data_independent <- fread(file.path(wd,"Datasources/file.csv"),stringsAsFactors=TRUE) # change to csv/rds name
data_dependent <- data_dependent <- readRDS(file.path(wd,"Datasources/county_dependent_vars.RDS")) #for county
# data_dependent <- data_dependent <- readRDS(file.path(wd,"Datasources/state_dependent_vars.RDS")) # for state

## if your data is not named properly, you may need to rename something to geoID/stateFIPS:
# setnames(data, "oldname", "geoID") - for county
# setnames(data, "oldname", "stateFIPS") - for state

dataset <- merge(data_independent, data_dependent, by="geoID", all.x=TRUE) # for county
# dataset <- merge(data_independent, data_dependent, by="stateFIPS", all.x=TRUE) # for state

y <- datatset$dependent # change to variable name
x <- dataset$independent # change to variable name

do_stuff <- function(x,y){
  plot(x,y)
  model <- lm(y ~ x)
  return(paste("slope =",round(coef(model)["x"],digits=4),
        "; r squared =",round(summary(model)$r.squared,digits=4)))
  }

do_stuff(x,y)
