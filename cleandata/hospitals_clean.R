# THIS SCRIPT gives you clean data of hospitals per county. It does nothing to clean those hospitals, unfortunately... 
# The final clean data object is: hospitals
# The main variables of interest are: # of beds and # of hospitals at the county level
# they are: beds_county, beds_state, hospitals_county, hospitals_state
#

library(data.table)
wd <- getwd()

# read

hospitals <- fread(file.path(wd,"Datasources/total_hospital.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- fread(file.path(wd,"Datasources/all_names_FIPS.csv"))
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

setnames(hospitals,"T_CO_Hos(total no. of hospitals in the county)","hospitals_county")
setnames(hospitals,"T_ST_HOS(Total no. of hospitals in the States)","hospitals_state")

# removing a lot of unneeded columns

data_states <- unique(data_states[,.(stateFIPS = as.factor(stateFIPS),state)])
hospitals <- hospitals[,.(geoID=as.factor(formatC(GEOID,width=5,flag="0")), stateFIPS=as.factor(formatC(STATEFP,width=2,flag="0")), 
                countyFIPS = COUNTYF, county = tolower(NAME), beds_county = Sum_BEDS, beds_state = Sum_BEDS_1, hospitals_county, 
                hospitals_state)]

hospitals <- merge(hospitals, data_states, by="stateFIPS", all.x=TRUE)

# clean
