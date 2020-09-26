# THIS SCRIPT gives you clean county-level community resilience data 
# (no added value for communities though, unfortunately)

# The final clean data object is: county_resilience
# The main fields of interest are: popn_low_risk, popn_med_risk, and popn_high_risk
# which define the # of people in each risk category at the county level

library(data.table)
wd <- getwd()

# read

county_resilience <- read.csv(file.path(wd,"Datasources/communityresilience_county.csv"),
                              stringsAsFactors=TRUE, header=TRUE)
data_states <- read.csv(file.path(wd,"Datasources/all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states <- as.data.table(data_states)
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

county_resilience <- county_resilience[-1,] # removing a second inner header

# naming standards

names <- names(county_resilience)
newnames <- c("couty_state","county","geoID","nation","stateFIPS","countyFIPS","census_tract",
              "population","popn_low_risk","popn_med_risk","popn_high_risk")
names(county_resilience) <- c(newnames,names[12:21])

# column reduction

county_resilience <- as.data.table(county_resilience)
county_resilience <- county_resilience[,.(couty_state,county,geoID = as.factor(geoID),
                        stateFIPS,countyFIPS,population,popn_low_risk,popn_med_risk,popn_high_risk)]

# adding standardized county, state, and code names

county_resilience <- merge(county_resilience,data_states,by=("geoID"),all.x=TRUE)
county_resilience <- county_resilience[,.(geoID,state,county=county.y,
                                stateFIPS=stateFIPS.x,countyFIPS=countyFIPS.x,
                                population, popn_low_risk,popn_med_risk,popn_high_risk)]

# ready to go!
