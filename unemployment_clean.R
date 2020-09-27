# THIS SCRIPT gives you clean data on montly 2020 unemployment per county. It does nothing to clean up the economy, unfortunately... 

# CHOOSE YOUR OWN ADVENTURE: you have two options for data format - unemployment_wide, or unemployment_long
# wide is the default, long just has one more step at the end that you can turn on or off
# wide and long refers to how the monthly unemployment rate is considered (jan - july 2020)

# The main variables of interest are: rate_month; or rate, and month (all data are 2020)

library(data.table)
library(tidyr)
wd <- getwd()

# read

unemployment <- fread(file.path(wd,"Datasources/county_unemploymentfebjuly2020.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- fread(file.path(wd,"Datasources/all_names_FIPS.csv"))
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

# extra header line

unemployment <- unemployment[-1,]

# short names

setnames(unemployment,"% Unemployed as of January 2020","rate_jan")
setnames(unemployment,"% Unemployed as of February 2020","rate_feb")
setnames(unemployment,"% Unemployed as of March 2020","rate_mar")
setnames(unemployment,"% Unemployed as of April 2020","rate_apr")
setnames(unemployment,"% Unemployed as of May 2020","rate_may")
setnames(unemployment,"% Unemployed as of June 2020","rate_jun")
setnames(unemployment,"% Unemployed as of July 2020","rate_jul")

# slim down columns

unemployment <- unemployment[,.(geoID = as.factor(FIPS),rate_jan,rate_feb,rate_mar,rate_apr,rate_may,rate_jun,rate_jul)]
unemployment <- merge(unemployment,data_states,by="geoID",all.x=TRUE)

unemployment_wide <- unemployment # this is the default

names <- names(unemployment)
names(unemployment) <- c(names[1],c("january","february","march","april","may",
                                    "june","july"),names[9:12])

months <- c("january","february","march","april","may",
            "june","july")

unemployment_long <- as.data.table(tidyr::gather(data=unemployment,key="month",
                                      value="rate",months,factor_key=TRUE))

# all set!
