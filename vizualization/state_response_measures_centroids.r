library(rgdal)
library(rgeos)
library(data.table)
library(lubridate)
library(raster)

#The Viz crew rocks!!!

#Set this master
wd <- 

states <- readOGR(file.path(wd,"Datasources/cb_2015_us_county_20m"), stringsAsFactors=TRUE)

data_states <- read.csv(file.path("cleandata/all_names_FIPS.csv"),stringsAsFactors=TRUE)

#From response_measures_clean.R
covid_measures <- fread("d:/jwindoc/NCSU_classes/Datamining/DC/COVID19_non-pharmaceutical-interventions_version2_utf8.csv", header=T, stringsAsFactors = T)

covid_measures <- covid_measures[,.(state=as.factor(tolower(State)),date=Date,Measure_L1,Measure_L2,Measure_L3,Measure_L4)]
covid_measures <- merge(covid_measures,data_states,by="state", all.x=TRUE)
covid_measures$date <- as.Date(covid_measures$date, format="%m/%d/%y")
measures <- c("Measure_L1","Measure_L2","Measure_L3","Measure_L4")
covid_measures_long <- as.data.table(tidyr::gather(data=covid_measures,key="number", value="measure",measures,factor_key=TRUE))
covid_measures_long[,month:=month(date)]
covid_measures_long <- covid_measures_long[,.(state,stateFIPS,date,month,measure)]
covid_measures_long[,.N,by=.(month,state)][order(-N)]

key_measures <- covid_measures_long[measure %in% c("Mass gathering cancellation","Small gathering cancellation","Closure of educational institutions","Travel restrictions",
  "Declare state of emergency","Closure of restaurants/bars/cafes","Closure of non-essential shops","Complete closure of primary and secondary schools",
  "Complete closure of kindergartens","Quarantine","National lockdown","Individual movement restrictions","Stay-at-home Order",
  "Mandatory home office","Face masks"),]

covid_measures_long[, week := strftime(week, format= %V)]

covid_measures_long[, week := week(ymd(week))]

#Move from long to wide starting here
covid_measures_long[, measures_per_week := .N , by = c("state", "week")]

covid_measures_long[, measures_per_day := .N , by = c("state", "date")]

#Subsetting because the formula in dcast only does one RHS per run.
cm1 <- covid_measures_long[, c("state", "stateFIPS", "week", "measures_per_week")]
cm2 <- covid_measures_long[, c("state", "stateFIPS", "date", "measures_per_day")]

#Actually widening
cov_m_wee <- dcast(cm1, state + stateFIPS ~ week, value.var = "measures_per_week")

cov_m_day <- dcast(cm2, state + stateFIPS ~ date, value.var = "measures_per_day")

names(cov_m_wee)[3:14] <- paste0("week_",names(cov_m_wee)[3:14])

names(cov_m_day)[3:65] <- paste0("date_",names(cov_m_day)[3:65])

cov_measures_cum <- cbind(cov_m_wee, cov_m_day)

cov_measures_cum <- cov_measures_cum[,-c(15,16)]

#Drop some locations that make the plotting the map annoying
states <- states[states$NAME != "Aleutians West",]

states <- states[states$STATEFP != "72",]

states$STATEFP <- droplevels(states$STATEFP)

states$STATEFP <- as.character(states$STATEFP)

state_dis <- gUnaryUnion(states, id = states$STATEFP)

state_centroids <- gCentroid(state_dis, byid = T)

cnet_s <- SpatialPointsDataFrame(state_centroids, data.frame(s_id = names(state_dis)))

covid_response_map <- merge(cnet_s, cov_measures_cum, by.x="s_id", by.y="stateFIPS", all.x=T)

plot(state_dis)
points(covid_response_map)


