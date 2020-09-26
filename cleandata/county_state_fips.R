library(rgdal)
library(data.table)
wd <- getwd()

# File to create the all_names_FIPS.csv crosswalk

states <- readOGR(file.path(wd,"Datasources/cb_2015_us_county_20m.shp"), stringsAsFactors=TRUE)
states_fips <-read.csv(file.path(wd,"Datasources/state-geocodes-v2017.csv"),stringsAsFactors=TRUE)

data_states <- as.data.table(states@data)
states_fips <- as.data.table(states_fips)

names(states_fips) <- c("region","div","stateFIPS","state")
names(data_states) <- c("stateFIPS","countyFIPS","NS","AFF","geoID","county","lsad","aland","awater")

states_fips$stateFIPS <- as.factor(formatC(states_fips$stateFIPS,width=2,flag="0"))

data_states <- merge(data_states,states_fips,by="stateFIPS", all.x=TRUE)
data_states <- data_states[,.(stateFIPS,countyFIPS,geoID,county,state)]
data_states$county <- tolower(data_states$county)
data_states$state <- tolower(data_states$state)

# write.csv(data_states,file.path(wd,"Datasources/all_names_FIPS.csv")) 

# unfortunately this removes leading 0's! :'( 
# so you need the following when reading in the data:

data_states <- read.csv(file.path(wd,"Datasources/all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states <- as.data.table(data_states)
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]
