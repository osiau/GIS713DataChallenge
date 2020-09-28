# THIS SCRIPT gives you clean data on US domestic flights. 
# It does nothing to improve airplane meals, unfortunately... 

# SUGGEST USING THE FINAL DATA FILES INSTEAD OF RUNNING THIS SCRIPT, IT MAY TAKE A WHILE
# Data files:
# flights_all.RDS - very cleaned up version of all flight data with arrivals and departures for each flight 

library(data.table)
library(tidyr)
wd <- getwd()

# read data beep boop

flights1 <- fread(file.path(wd,"Datasources/DEC2019_T_ONTIME_REPORTING.csv"), header=TRUE)
flights2 <- fread(file.path(wd,"Datasources/JAN2020_T_ONTIME_REPORTING.csv"), header=TRUE)
flights3 <- fread(file.path(wd,"Datasources/FEB2020_T_ONTIME_REPORTING.csv"), header=TRUE)
flights4 <- fread(file.path(wd,"Datasources/MAR2020_T_ONTIME_REPORTING.csv"), header=TRUE)
flights5 <- fread(file.path(wd,"Datasources/APR2020_T_ONTIME_REPORTING.csv"), header=TRUE)
flights6 <- fread(file.path(wd,"Datasources/MAY2020_T_ONTIME_REPORTING.csv"), header=TRUE)
flights7 <- fread(file.path(wd,"Datasources/JUN2020_T_ONTIME_REPORTING.csv"), header=TRUE)

data_states <- fread(file.path(wd,"Datasources/all_names_FIPS.csv"))
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

# all.equal(names(flights4),names(flights5))
# Dec 2019 data has different column names than the rest, fun!

flights2_7 <- rbind(flights2,flights3,flights4,flights5,flights6,flights7)
flights2_7 <- flights2_7[,.(FL_DATE,ORIGIN_CITY_NAME,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,
                            DEST_CITY_NAME,DEST_STATE_FIPS,DEST_STATE_NM)]
flights1 <- flights1[,.(FL_DATE,ORIGIN_CITY_NAME,ORIGIN_STATE_FIPS,ORIGIN_STATE_NM,
                        DEST_CITY_NAME,DEST_STATE_FIPS,DEST_STATE_NM)]

flights_all <- rbind(flights2_7,flights1)
remove(flights1,flights2,flights3,flights4,flights5,flights6,flights7,flights2_7)
flights_all$DEST_STATE_FIPS <- as.factor(formatC(flights_all$DEST_STATE_FIPS,width=2,flag="0"))
flights_all$ORIGIN_STATE_FIPS <- as.factor(formatC(flights_all$ORIGIN_STATE_FIPS,width=2,flag="0"))
flights_all <- separate(flights_all,ORIGIN_CITY_NAME,into= c("origin","X"),sep=",")
flights_all <- separate(flights_all,DEST_CITY_NAME,into= c("dest","X1"),sep=",")

 # just one table: flights_all

flights_all <- flights_all[,.(date=FL_DATE,origin_county=tolower(origin),
                              origin_stateFIPS=ORIGIN_STATE_FIPS,origin_state=tolower(ORIGIN_STATE_NM),
                              dest_county=tolower(dest),dest_stateFIPS=DEST_STATE_FIPS,dest_state=tolower(DEST_STATE_NM))]


flight_pairs_month <- flights_all[,.N,by=.(month=month(date),origin_state,dest_state)]
# write.csv(flight_pairs_month,file.path(wd,"Datasources/flights_pairs_month.csv"))

# saveRDS(flights_all,file.path(wd,"Datasources/flights_all.RDS"))
