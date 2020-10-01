# Script to find the best regression fits for state-level per-capita cases, deaths, and unemployment

library(data.table)
library(RCurl)
library(httr)
library(bit64)
library(readr)
library(mosaic)
library(Stat2Data)
library(dplyr)
library(corrplot)
library(car)
library(leaps)
library(bestglm)
library(tidyr)
library(tidycensus)
library(tigris)
library(sf)
library(MASS)
library(raster)
library(spdep)
library(rgdal)
library(maptools)
library(gridExtra)
library(rgeos)

data_dir <- "/Users/ihinks/Documents/GIS713/DataChallenge"
setwd(data_dir)
wd <- getwd()

# Read data

# Get shapefile
county_shp <- shapefile(file.path(wd, "Datasources/cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
states <- aggregate(county_shp, by="STATEFP")
# 1- county-level COVID19
state_dem_data <- fread(file.path(wd,"regression supplies/state_dem_votes_and_covid.csv"))
state_covid_votes <- fread(file.path(wd,"regression supplies/state_votes_and_covid.csv"))

# Dependent vars
state_var <- readRDS("regression supplies/state_dependent_vars.RDS")

# Gathering tourism data ##############################################################################################
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
# Aggregate tourism by state
tourism_agg <- tourism_wide[, .(tourism = sum(jan, feb, mar, apr, may, jun, jul)), by=.(state, stateFIPS)]

# Gathering flights data #######################################################################################################
flights <- fread(file.path(wd,"cleandata/flights_pairs_month.csv"),stringsAsFactors=TRUE, header=TRUE)
# Num of arrivals by state
flights_agg <- flights[, .(flight_arrivals = sum(N)), by=.(dest_state)]
setnames(flights_agg, "dest_state", "state")

# Gathering government response data ############################################################################################
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

# Number of measures by state
key_measures_agg <- key_measures[, .(num_measures = .N), by=.(state)]

# Gathering state-level hospital data #######################################################################################################

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

hospitals_state <- hospitals[, .(beds_state = mean(beds_state), hospitals_state = mean(hospitals_state)), by=(stateFIPS)]

# Gathering Census data ###########################################################################################################

#for ACS data 1st get a census API key ->  https://api.census.gov/data/key_signup.html
census_api_key("2e74a427e023c18449dfc52e92d32f345c24832c", install = FALSE) #,install=TRUE only for the first time you use it 
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")

# define factors
# codes for factors can be found using e.g. v18 <- load_variables(2018, "acs5", cache = TRUE)
factors <- c(
  total_pop = "B01003_001",
  pub_transport = "B08101_025",
  med_income = "B07011_001",
  pop_white = "B02001_002",
  pop_black = "B02001_003",
  pop_amindian = "B02001_004",
  pop_asian = "B02001_005",
  pop_pacislander = "B02001_006",
  pop_otherrace = "B02001_007",
  pop_multirace = "B02001_008",
  median_age = "B01002_001",
  pop_under18 = "B09001_001",
  pop_over65 = "B09021_022",
  pop_insured = "B27001_001"
)


# read in the ACS data for 2018 for the listed factors
acsdata <- get_acs(
  geography = "county",
  variables = factors,
  year = 2018,
  survey = "acs5",
  geometry = F
)

# separate the county and state names (though note that the word "county" is still included in each name)
acsdata$county <- tolower(unlist(strsplit(acsdata$NAME,"[,]"))[seq(1,length(unlist(strsplit(acsdata$NAME,"[,]"))),2)])
acsdata$state <- tolower(unlist(strsplit(acsdata$NAME,"[,]"))[seq(2,length(unlist(strsplit(acsdata$NAME,"[,]"))),2)])
acsdata$state <- substr(acsdata$state,2,nchar(acsdata$state))

# convert to a data table
acsdata <- data.table(acsdata)
# ignore puerto rico
acsdata <- acsdata[state != "puerto rico"]

# manually convert to wide format
var_names <- unique(acsdata$variable)
for (v in 1:length(var_names)) {
  temp_singlevar <- acsdata[variable==var_names[v]]
  # with the first iteration, define the county names, geoids, geometries
  if (v == 1) {
    acs_wide <- data.table()
    acs_wide$geoid <- temp_singlevar$GEOID
    acs_wide$county <- temp_singlevar$county
    acs_wide$state <- temp_singlevar$state
    #acs_wide$geometry <- temp_singlevar$geometry
  }
  acs_wide[,var_names[v]] <- temp_singlevar$estimate
}

# Calculate percentages of total population for relevant variables
acs_wide$frac_white <- acs_wide[,pop_white]/acs_wide[,total_pop]
acs_wide$frac_black <- acs_wide[,pop_black]/acs_wide[,total_pop]
acs_wide$frac_amindian <- acs_wide[,pop_amindian]/acs_wide[,total_pop]
acs_wide$frac_asian <- acs_wide[,pop_asian]/acs_wide[,total_pop]
acs_wide$frac_pacislander <- acs_wide[,pop_pacislander]/acs_wide[,total_pop]
acs_wide$frac_otherrace <- acs_wide[,pop_otherrace]/acs_wide[,total_pop]
acs_wide$frac_multirace <- acs_wide[,pop_multirace]/acs_wide[,total_pop]
acs_wide$frac_under18 <- acs_wide[,pop_under18]/acs_wide[,total_pop]
acs_wide$frac_over65 <- acs_wide[,pop_over65]/acs_wide[,total_pop]
acs_wide$frac_insured <- acs_wide[,pop_insured]/acs_wide[,total_pop]
acs_wide$frac_pubtransport <- acs_wide[,pub_transport]/acs_wide[,total_pop] # this is the fraction of people who use public transportation to get to work

# finally, remove unnecessary columns (i.e. totals)
acs_final <- acs_wide[,.(geoid,county,state, median_age,med_income,frac_white,
                         frac_black,frac_amindian,frac_asian,frac_pacislander,
                         frac_otherrace,frac_under18,frac_over65,frac_insured,
                         frac_pubtransport)]

# Completed

# Harmonized geID names
setnames(acs_final, "geoid", "geoID")

cols_chosen <- c("median_age","med_income","frac_white",
                 "frac_black","frac_amindian","frac_asian","frac_pacislander",
                 "frac_otherrace","frac_under18","frac_over65","frac_insured",
                 "frac_pubtransport")
acs_agg <- acs_final[, lapply(.SD, mean), by=.(state), .SDcols = cols_chosen]

###################################################################################################################################################################

# Merge data
all_state_data <- merge(state_var, tourism_agg, by="stateFIPS", all=T)
all_state_data <- merge(all_state_data, hospitals_state, by="stateFIPS")
all_state_data <- all_state_data[, state := NULL]
all_state_data <- merge(all_state_data, data_states, by="stateFIPS", all.x=T)
all_state_data <- merge(all_state_data, flights_agg, by="state", all=T)
all_state_data <- merge(all_state_data, key_measures_agg, by="state", all=T)
all_state_data <- merge(all_state_data, acs_agg, by="state", all.x=T)

# Calculating percent of total votes earned by democratic candidates
dem_vote_pct <- state_dem_data[, .(dem_vote_pct = vote_pct), by=stateFIPS]
dem_vote_pct$stateFIPS <- as.factor(as.character(sprintf("%02d", dem_vote_pct$stateFIPS)))
all_state_data <- merge(all_state_data, dem_vote_pct, by="stateFIPS", all.x=T)
write.csv(all_state_data, "state_dep_and_indep.csv")

scatter.smooth(all_state_data$total_cases_pc~all_state_data$dem_vote_pct)
scatter.smooth(all_state_data$total_deaths_pc~all_state_data$dem_vote_pct)

#####################################################################

# Test models

# All variables

state_var_all <- all_state_data[,.(vote_pct, dem_vote_pct, total_cases_pc, total_deaths_pc, unemploy, 
                                   tourism, beds_state, hospitals_state, flight_arrivals, num_measures, 
                                   median_age, med_income, frac_white, frac_black, frac_amindian, 
                                   frac_asian, frac_pacislander, frac_otherrace, frac_under18, frac_over65, 
                                   frac_insured, frac_pubtransport)]
# Scatter plots and correlations coefficents between all pairs of variables
round(cor(state_var_all, method="pearson", use = "complete.obs"), 3)
pairs(state_var_all)

# Test statistic for the strongest correlations
# Cases
cor.test(state_var_all$total_cases_pc, state_var_all$frac_asian, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$med_income, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$frac_pacislander, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$frac_black, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$dem_vote_pct, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$frac_under18, method = "spearman") 
cor.test(state_var_all$total_cases_pc, state_var_all$frac_insured, method = "spearman") 
# Deaths
cor.test(state_var_all$total_deaths_pc, state_var_all$frac_pubtransport, method = "spearman") 
cor.test(state_var_all$total_deaths_pc, state_var_all$frac_white, method = "spearman")
cor.test(state_var_all$total_deaths_pc, state_var_all$frac_pacislander, method = "spearman") 
cor.test(state_var_all$total_deaths_pc, state_var_all$frac_asian, method = "spearman") 
# Unemployment
cor.test(state_var_all$unemploy, state_var_all$median_age, method = "spearman") 
cor.test(state_var_all$unemploy, state_var_all$frac_under18, method = "spearman") 
cor.test(state_var_all$unemploy, state_var_all$frac_pacislander, method = "spearman") 
cor.test(state_var_all$unemploy, state_var_all$frac_asian, method = "spearman") 
cor.test(state_var_all$unemploy, state_var_all$med_income, method = "spearman") 

# Model with all predictors
# Cases
lm_model_covid_cases <- lm(total_cases_pc ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                             med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                             frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_cases) # Only frace_white, frac_black, frac_amindian
# Deaths
lm_model_covid_deaths <- lm(total_deaths_pc ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                             med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                             frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_deaths) # Only intercept is significant
# Unemployment
lm_model_covid_unemploy <- lm(unemploy ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                             med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                             frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_unemploy) # Only intercept is significant

# NOTE: Log transformation on total_cases_pc better

# VIF test for multicollinearity
# Evaluate Collinearity

vif(lm_model_covid_cases)  # variance inflation factors
sqrt(vif(lm_model_covid_cases)) > 2  # all but num_measures indicate multicollinearity

vif(lm_model_covid_deaths)  # variance inflation factors
sqrt(vif(lm_model_covid_deaths)) > 2  # all but num_measures indicate multicollinearity

vif(lm_model_covid_unemploy)  # variance inflation factors
sqrt(vif(lm_model_covid_unemploy)) > 2  # all but num_measures indicate multicollinearity

# Model 2, without some variables with multicollinearity
# Cases 
lm_model_covid_cases_v2 <- lm(total_cases_pc ~ frac_asian + med_income + frac_pacislander + frac_black + dem_vote_pct + frac_under18 + frac_insured, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_cases_v2) # adj R^2: 0.5356, p-value: 1.254e-06
vif(lm_model_covid_cases_v2)
# Removing significant multicollinearity
lm_model_covid_cases_v3 <- lm(total_cases_pc ~ med_income + frac_black + dem_vote_pct + frac_under18 + frac_insured, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_cases_v3) # adj R^2: 0.5389, p-value: 2.134e-07
vif(lm_model_covid_cases_v3)

# Deaths
lm_model_covid_deaths_v2 <- lm(total_deaths_pc ~ frac_pubtransport + frac_white + frac_pacislander + frac_asian, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_deaths_v2) # adj R^2: 0.266, p-value: 0.001166
vif(lm_model_covid_deaths_v2)
# Removing significant multicollinearity
lm_model_covid_deaths_v3 <- lm(total_deaths_pc ~ frac_pubtransport + frac_white + frac_pacislander, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_deaths_v3) # adj R^2: 0.1377, p-value: 0.02012
vif(lm_model_covid_deaths_v3)

# Unemployment
lm_model_covid_unemploy_v2 <- lm(unemploy ~ median_age + frac_under18 + frac_pacislander + med_income, data=state_var_all, na.action = na.exclude)
summary(lm_model_covid_unemploy_v2) # adj R^2: 0.1815, p-value: 0.01169
vif(lm_model_covid_unemploy_v2)

# AIC test
# Cases
model_all_var_cases <-regsubsets(total_cases_pc ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                                   med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                                   frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, 
                                   nbest=10, really.big=T, intercept=F)
all.mods.cases <- summary(model_all_var_cases)[[1]]
all.mods.cases <- lapply(1:nrow(all.mods.cases), function(x) as.formula(paste("total_cases_pc~", paste(names(which(all.mods.cases[x,])), collapse="+"))))
#all.mods.cases
# Deaths
model_all_var_deaths <-regsubsets(total_deaths_pc ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                                    med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                                    frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, 
                                  nbest=10, really.big=T, intercept=F)
all.mods.deaths <- summary(model_all_var_deaths)[[1]]
all.mods.deaths <- lapply(1:nrow(all.mods.deaths), function(x) as.formula(paste("total_deaths_pc~", paste(names(which(all.mods.deaths[x,])), collapse="+"))))
#all.mods.deaths
# Unemployment
model_all_var_unemploy <-regsubsets(unemploy ~ beds_state + hospitals_state + flight_arrivals + num_measures + median_age + 
                                      med_income + frac_white + frac_black + frac_amindian + frac_asian + frac_pacislander + frac_otherrace + 
                                      frac_under18 + frac_over65 + frac_insured + frac_pubtransport, data=state_var_all, 
                                    nbest=10, really.big=T, intercept=F)
all.mods.unemploy <- summary(model_all_var_unemploy)[[1]]
all.mods.unemploy <- lapply(1:nrow(all.mods.unemploy), function(x) as.formula(paste("unemploy~", paste(names(which(all.mods.unemploy[x,])), collapse="+"))))
#all.mods.unemploy

# AIC analysis:
# Cases
all.lm.cases<-lapply(all.mods.cases, lm, state_var_all)
sapply(all.lm.cases, extractAIC)[2,]
min(sapply(all.lm.cases, extractAIC)[2,])
# Deaths
all.lm.deaths<-lapply(all.mods.deaths, lm, state_var_all)
sapply(all.lm.deaths, extractAIC)[2,]
min(sapply(all.lm.deaths, extractAIC)[2,])
# Unemployment
all.lm.unemploy<-lapply(all.mods.unemploy, lm, state_var_all)
sapply(all.lm.unemploy, extractAIC)[2,]
min(sapply(all.lm.unemploy, extractAIC)[2,])

# Model with lower AIC: 
# total_cases_pc ~ hospitals_state

# Test new model
lm_model_covid_cases_v4 <- lm((total_cases_pc) ~ frac_black + frac_under18 + frac_over65 + frac_insured, data=state_var_all)
summary(lm_model_covid_cases_v4) # adj R^2: 0.5437, p-value: 3.061e-08

lm_model_covid_deaths_v4 <- lm(log(total_deaths_pc) ~ (frac_otherrace), data=state_var_all)
summary(lm_model_covid_deaths_v4) # adj R^2: 0.0634, p-value: 0.04028

lm_model_covid_unemploy_v4 <- lm((unemploy) ~ (hospitals_state) + (num_measures) + (med_income) + (frac_black) + (frac_pacislander) + (frac_otherrace) + (frac_insured) + (frac_pubtransport), data=state_var_all)
summary(lm_model_covid_unemploy_v4) # adj R^2: 0.3936, p-value: 0.03756

# Improve with transformations
lm_model_covid_cases_v5 <- lm((total_cases_pc) ~ log(frac_black) + (frac_under18) + sqrt(frac_over65) + log(frac_insured), data=state_var_all)
summary(lm_model_covid_cases_v5) # adj R^2: 0.5972, p-value: 1.877e-09

lm_model_covid_deaths_v5 <- lm(log(total_deaths_pc) ~ sqrt(frac_otherrace), data=state_var_all)
summary(lm_model_covid_deaths_v5) # adj R^2: 0.1236, p-value: 0.006601

lm_model_covid_unemploy_v5 <- lm(log(unemploy) ~ (hospitals_state) + (num_measures) + (med_income) + log(frac_black) + (frac_pacislander) + (frac_otherrace) + log(frac_insured) + (frac_pubtransport), data=state_var_all)
summary(lm_model_covid_unemploy_v5) # adj R^2: 0.3985, p-value: 0.03579

plot(lm_model_covid_cases_v5)
plot(lm_model_covid_deaths_v5)
plot(lm_model_covid_unemploy_v5)

# Diagnostic
# (1) Influential Observations
# Cases
avPlots(lm_model_covid_cases_v5)
cutoff_cases = 4/((nrow(state_var_all)-length(lm_model_covid_cases_v5$coefficients)-2))
plot(lm_model_covid_cases_v5,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_cases_v5,main="Influence Plot for Cases",sub="circle size proportional to Cook's Distance")
# Deaths
avPlots(lm_model_covid_deaths_v5)
cutoff_cases = 4/((nrow(state_var_all)-length(lm_model_covid_deaths_v5$coefficients)-2))
plot(lm_model_covid_deaths_v5,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_deaths_v5,main="Influence Plot for Cases",sub="circle size proportional to Cook's Distance")
# Unemployment
avPlots(lm_model_covid_unemploy_v5)
cutoff_cases = 4/((nrow(state_var_all)-length(lm_model_covid_unemploy_v5$coefficients)-2))
plot(lm_model_covid_unemploy_v5,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_unemploy_v5,main="Influence Plot for Cases",sub="circle size proportional to Cook's Distance")

# Cases
outlierTest(lm_model_covid_cases_v5)
leveragePlots(lm_model_covid_cases_v5)
influence.measures(lm_model_covid_cases_v5)
# Deaths
outlierTest(lm_model_covid_deaths_v5)
leveragePlots(lm_model_covid_deaths_v5)
influence.measures(lm_model_covid_deaths_v5)
# Unemployment
outlierTest(lm_model_covid_unemploy_v5)
leveragePlots(lm_model_covid_unemploy_v5)
influence.measures(lm_model_covid_unemploy_v5)

# NOTE: Cook's Dostance reveals 6 influencial observations: 60, 148, 156, 205, 208, 312. 
# 156 and 312 are the most extreme values. We can try to remove them

# (2) Evaluate Normality
# qq plot for studentized residuals
qqPlot(lm_model_covid_cases_v5,main="QQ Plot for Cases")
qqPlot(lm_model_covid_deaths_v5,main="QQ Plot for Deaths")
qqPlot(lm_model_covid_unemploy_v5,main="QQ Plot for Unemployment")


# Cases
sresid_cases = studres(lm_model_covid_cases_v5)
hist(sresid_cases, freq=FALSE, main="Distribution of Studentized Residuals for Cases")
xfit_cases = seq(min(sresid_cases),max(sresid_cases),length=40)
yfit_cases = dnorm(xfit_cases)
lines(xfit_cases, yfit_cases)
# Deaths
sresid_deaths = studres(lm_model_covid_deaths_v5)
hist(sresid_deaths, freq=FALSE, main="Distribution of Studentized Residuals for Deaths")
xfit_deaths = seq(min(sresid_deaths),max(sresid_deaths),length=40)
yfit_deaths = dnorm(xfit_deaths)
lines(xfit_deaths, yfit_deaths)
# Unemployment
sresid_unemploy = studres(lm_model_covid_unemploy_v5)
hist(sresid_unemploy, freq=FALSE, main="Distribution of Studentized Residuals for Unemployment")
xfit_unemploy = seq(min(sresid_unemploy),max(sresid_unemploy),length=40)
yfit_unemploy = dnorm(xfit_unemploy)
lines(xfit_unemploy, yfit_unemploy)

# (3) Evaluate homoscedasticity
# Cases
ncvTest(lm_model_covid_cases_v5)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model_covid_cases_v5)
# Deaths
ncvTest(lm_model_covid_deaths_v5)  
# plot studentized residuals vs. fitted values
#spreadLevelPlot(lm_model_covid_deaths_v5)
# Unemployment
ncvTest(lm_model_covid_unemploy_v5)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model_covid_unemploy_v5)

# (4) Evaluate Independence of errors
# DW test: null hypothesis: no autocorrelation -- fail to reject
durbinWatsonTest(lm_model_covid_cases_v5) # Suggested power transformation: 0.1435897 
durbinWatsonTest(lm_model_covid_deaths_v5) 
durbinWatsonTest(lm_model_covid_unemploy_v5) 

# Check quadratic model :
quadratic_covid_cases_v5 <- lm((total_cases_pc) ~ (frac_black)^2 + (frac_under18)^2 + (frac_over65)^2 + (frac_insured)^2, data=state_var_all)
summary(quadratic_covid_cases_v5) # adj R^2: 0.5437; p-value: 3.061e-08

quadratic_covid_deaths_v5 <- lm(log(total_deaths_pc) ~ (frac_otherrace)^2, data=state_var_all)
summary(quadratic_covid_deaths_v5) # adj R^2: 0.08016; p-value: 0.02487

quadratic_covid_unemploy_v5 <- lm(log(unemploy) ~ (hospitals_state)^2 + (num_measures)^2 + (med_income)^2 + (frac_black)^2 + (frac_pacislander)^2 + (frac_otherrace)^2 + (frac_insured)^2 + (frac_pubtransport)^2, data=state_var_all)
summary(quadratic_covid_unemploy_v5) # adj R^2: 0.3524; p-value: 0.05517


# Test glm
# Cases
glm_model_covid_cases_v5 <- glm((total_cases_pc) ~ (frac_black) + (frac_under18) + sqrt(frac_over65) + (frac_insured), data=state_var_all, family = binomial(link="logit"))
summary(glm_model_covid_cases_v5)
# Deaths
glm_model_covid_deaths_v5 <- glm((total_deaths_pc) ~ sqrt(frac_otherrace), 
                                data= state_var_all, family = binomial(link="logit"))
summary(glm_model_covid_deaths_v5)
# Unemployment
glm_model_covid_deaths_v5 <- glm((unemploy) ~ (hospitals_state) + (num_measures) + (med_income) + (frac_black) + (frac_pacislander) + (frac_otherrace) + (frac_insured) + (frac_pubtransport), 
                                 data= state_var_all, family = binomial(link="logit"))
summary(glm_model_covid_deaths_v5)

# Test beta (logit)
cases_beta <- betareg((total_cases_pc) ~ (frac_black) + (frac_under18) + sqrt(frac_over65) + (frac_insured), data=state_var_all)
summary(cases_beta) # None significant; pseudo-R^2 of 0.1432
plot(cases_beta)
deaths_beta <- betareg((total_deaths_pc) ~ sqrt(frac_otherrace), data=state_var_all)
summary(deaths_beta) # dem_vote_pct and beds_state are significant
plot(deaths_beta)

###### Spatial autocorrelation ######
autoCorrelation <- function(shapeFile, indVariable, model)
{
  w <- 1/ as.matrix(dist(coordinates(shapeFile)))
  diag(w) <- 0
  print(moran.test(indVariable, mat2listw(w)))
  print(moran.test(residuals.glm(model), mat2listw(w)))
}

states_relevantVars <- all_state_data[, .(stateFIPS, total_cases_pc, total_deaths_pc, unemploy, frac_black, frac_under18, frac_over65, frac_insured, frac_otherrace, hospitals_state, num_measures, med_income, frac_pacislander, frac_pubtransport)]
states_noNA <- na.omit(states_relevantVars)
states_subset <- subset(states, states@data$STATEFP %in% as.character(states_noNA$stateFIPS))

autoCorrelation(states_subset, states_noNA$total_cases_pc, lm_model_covid_cases_v5)
autoCorrelation(states_subset, states_noNA$total_deaths_pc, lm_model_covid_cases_v5)
autoCorrelation(states_subset, states_noNA$unemploy, lm_model_covid_cases_v5)
