
# THIS SCRIPT gives you clean air quality data (not clean air quality though, unfortunately)
# The final clean data objects are: air_qual2019fp and air_qual2020_fp
# No calculation has been done on the air quality fields

library(data.table)
wd <- getwd()

# read

air_qual2019 <- read.csv(file.path(wd,"Datasources/air_quaity_annual_aqi_by_county_2019.csv"),stringsAsFactors=TRUE, header=TRUE)
air_qual2020 <- read.csv(file.path(wd,"Datasources/air_quality_annual_aqi_by_county_2020.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- read.csv(file.path(wd,"Datasources/all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states <- as.data.table(data_states)
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

# convert to data.table

air_qual2019 <- as.data.table(air_qual2019)
air_qual2020 <- as.data.table(air_qual2020)

# lowercase names

names(air_qual2019) <- tolower(names(air_qual2019))
names(air_qual2020) <- tolower(names(air_qual2020))

summary(data_states)

# adding codes to air quality

air_qual2019$state <- tolower(air_qual2019$state)
air_qual2019$county <- tolower(air_qual2019$county)
air_qual2019 <- merge(air_qual2019,data_states,by=c("state","county"))

air_qual2020$state <- tolower(air_qual2020$state)
air_qual2020$county <- tolower(air_qual2020$county)
air_qual2020 <- merge(air_qual2020,data_states,by=c("state","county"))
