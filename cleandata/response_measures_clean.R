# THIS SCRIPT gives you clean data about non-pharmaceutical response measures taken by state governments
# it does nothing however to clean up government spending :(
# The final clean data object is: covid_measures
# You can use it to see how many measures were instated and on what timelines, but there is quite 
# a mix of measures included (columns: Measure_L1, MeasureL2, Measure_L3, Measure_L4.

# So far this is just cleaned to be interoperable with state/dates, next step will be to see how 
# to classify the measure data

library(data.table)
wd <- getwd()

# read

covid_measures <- read.csv(file.path(wd,"Datasources/COVID19_non-pharmaceutical-interventions_version2_utf8.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- read.csv(file.path(wd,"Datasources/all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states <- as.data.table(data_states)
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

# convert to data.table

covid_measures <- as.data.table(covid_measures)

# select columns

data_states <-unique(data_states[,.(state,stateFIPS)])
covid_measures <- covid_measures[,.(state=as.factor(tolower(State)),date=Date,Measure_L1,Measure_L2,Measure_L3,Measure_L4)]

# add stateFIPS

covid_measures <- merge(covid_measures,data_states,by="state", all.x=TRUE)
covid_measures$date <- as.Date(covid_measures$date, format="%m/%d/%y")

# that's it
