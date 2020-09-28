# THIS SCRIPT gives you clean data about non-pharmaceutical response measures taken by state governments
# it does nothing however to clean up government spending :(
# The final clean data object is: covid_measures OR covid_measures_long OR key_measures (a subset list of measures)

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

# long format

measures <- c("Measure_L1","Measure_L2","Measure_L3","Measure_L4")
covid_measures_long <- as.data.table(tidyr::gather(data=covid_measures,key="number",
                                                 value="measure",measures,factor_key=TRUE))

covid_measures_long[,month:=month(date)]
covid_measures_long <- covid_measures_long[,.(state,stateFIPS,date,month,measure)]

covid_measures_long[,.N,by=.(month,state)][order(-N)]
cml <- covid_measures_long
summary(as.factor(cml$measure))

key_measures <- covid_measures_long[measure %in% c("Mass gathering cancellation","Small gathering cancellation","Closure of educational institutions","Travel restrictions",
  "Declare state of emergency","Closure of restaurants/bars/cafes","Closure of non-essential shops","Complete closure of primary and secondary schools",
  "Complete closure of kindergartens","Quarantine","National lockdown","Individual movement restrictions","Stay-at-home Order",
  "Mandatory home office","Face masks"),]

key_measures$type <- as.factor(ifelse(key_measures$measure %in% c("Mass gathering cancellation","Small gathering cancellation"),"Cancel gatherings",
                              ifelse(key_measures$measure %in% c("Closure of educational institutions","Complete closure of primary and secondary schools","Complete closure of kindergartens"), "School closure",
                              ifelse(key_measures$measure %in% c("Travel restrictions","Individual movement restrictions"),"Restrict mobility",
                              ifelse(key_measures$measure %in% c("Declare state of emergency","Quarantine","National lockdown","Stay-at-home Order"),"Lockdown",
                              ifelse(key_measures$measure %in% c("Mandatory home office","Closure of restaurants/bars/cafes","Closure of non-essential shops"), "Business closure",
                              ifelse(key_measures$measure=="Face masks", "Masks","Other")))))))
