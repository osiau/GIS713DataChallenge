# THIS SCRIPT gives you clean data on arrivals (# passenges) to top international tourist destinations. 

# CHOOSE YOUR OWN ADVENTURE: you have two options for data format - tourism_wide, or tourism_long
# wide is the default, long just has one more step at the end that you can turn on or off
# wide and long refers to how the monthly arrival rate is considered (jan - july 2020)

# The main variables of interest are: month; or month, arrivals

library(data.table)
library(tidyr)
wd <- getwd()

# read

tourism <- fread(file.path(wd,"Datasources/Overseas arrivals.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- fread(file.path(wd,"Datasources/all_names_FIPS.csv"))
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]
state_abvr <- fread(file.path(wd,"Datasources/state_abvr.csv"),stringsAsFactors=TRUE)
state_abvr <- state_abvr[,.(state=tolower(state),abvr=tolower(abvr))]

data_states <- merge(data_states,state_abvr,by="state",all.x=TRUE)
data_states <- unique(data_states[,.(abvr,state,stateFIPS)])

# Como no! No mas comas

comma_no <- function(x){
  as.numeric(gsub(",","",x))
}

tourism <- tourism[,lapply(.SD, comma_no),by=Port,.SDcols=names(tourism)[2:9]]
tourism <- separate(tourism,Port,into= c("county","state"),sep=", ")

names(tourism) <- c(names(tourism)[1:2],c("jan","feb","mar","apr","may","jun","x","jul"))

# compatibility with data states

tourism <- tourism[,.(county=tolower(county),state=tolower(state),.SD),.SDcols=jan:jul]
names(tourism) <- c("airport","abvr","jan","feb","mar","apr","may","jun","x","jul")

tourism <- merge(tourism,data_states,by="abvr",all.x=TRUE)
tourism <- tourism[,.(airport,state,stateFIPS,jan,feb,mar,apr,may,jun,jul)]

# wide and long

tourism_wide <- tourism
months <- c("jan","feb","mar","apr","may","jun","jul")
tourism_long <- as.data.table(tidyr::gather(data=tourism,key="month",
                                value="intl_arrivals",months,factor_key=TRUE))

# clean!
