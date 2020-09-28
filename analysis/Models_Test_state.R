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

data_dir <- "/Users/ihinks/Documents/GIS713/DataChallenge"
setwd(data_dir)
wd <- getwd()

# Read data

# 1- county-level COVID19
state_dem_data <- fread(file.path(wd,"regression supplies/state_dem_votes_and_covid.csv"))
state_covid_votes <- fread(file.path(wd,"regression supplies/state_votes_and_covid.csv"))
state_dem_data_
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
census_api_key("2e74a427e023c18449dfc52e92d32f345c24832c", install = TRUE) #,install=TRUE only for the first time you use it 
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



###################################################################################################################################################################

# Merge data
all_state_data <- merge(flights_agg, tourism_agg, by="state")
all_state_data <- merge(all_state_data, key_measures_agg, by="state")
#all_state_data <- merge(state_var, all_state_data, by="stateFIPS")
all_state_data <- merge(state_var, hospitals_state, by="stateFIPS")

# Calculating percent of total votes earned by democratic candidates
dem_vote_pct <- state_dem_data[, .(dem_vote_pct = vote_pct), by=stateFIPS]
dem_vote_pct$stateFIPS <- as.factor(as.character(dem_vote_pct$stateFIPS))
all_state_data <- merge(all_state_data, dem_vote_pct, by="stateFIPS")

scatter.smooth(all_state_data$total_cases_pc~all_state_data$dem_vote_pct)
scatter.smooth(all_state_data$total_deaths_pc~all_state_data$dem_vote_pct)

#####################################################################

# Test models

# All variables

state_var_all <- all_state_data[,.(vote_pct, dem_vote_pct, total_cases_pc, total_deaths_pc, unemploy, beds_state, hospitals_state)]
# Scatter plots and correlations coefficents between all pairs of variables
round(cor(state_var_all, method="pearson", use = "complete.obs"), 3)
pairs(state_var_all)

# Test statistic for the strongest correlations
# Cases
cor.test(state_var_all$total_cases_pc, state_var_all$beds_state, method = "spearman")
cor.test(state_var_all$total_cases_pc, state_var_all$hospitals_state, method = "spearman")
# Deaths
cor.test(state_var_all$total_deaths_pc, state_var_all$dem_vote_pct, method = "spearman")
cor.test(state_var_all$total_deaths_pc, state_var_all$beds_state, method = "spearman")

# Model with all predictors

lm_model_covid_cases <- lm(total_cases_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
summary(lm_model_covid_cases) # Only intercept is significant
lm_model_covid_deaths <- lm(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
summary(lm_model_covid_deaths) # dem_vote_pct is significant

# NOTE: Log transformation on total_cases_pc better


# VIF test for multicollinearity
# Evaluate Collinearity

vif(lm_model_covid_cases)  # variance inflation factors
sqrt(vif(lm_model_covid_cases)) > 2  # beds_state and hospitals_state indicate multicollinearity

vif(lm_model_covid_deaths)  # variance inflation factors
sqrt(vif(lm_model_covid_deaths)) > 2  # beds_state and hospitals_state indicate multicollinearity

# NOTE: beds_state and hospitals_state > 5 - Let's remove them

# Model 2 without beds_state or hospitals_state
# Cases 
lm_model_covid_cases_v2 <- lm(total_cases_pc ~ dem_vote_pct + unemploy, data=state_var_all)
summary(lm_model_covid_cases_v2) # adj R^2: -0.03792, p-value: 0.8078
vif(lm_model_covid_cases_v2)
# Deaths
lm_model_covid_deaths_v2 <- lm(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state, data=state_var_all)
summary(lm_model_covid_deaths_v2) # adj R^2: 0.2288, p-value: 0.00377
vif(lm_model_covid_deaths_v2)

# AIC test
# Cases
model_all_var_cases <-regsubsets(total_cases_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all, nbest=10, really.big=T, intercept=F)
all.mods.cases <- summary(model_all_var_cases)[[1]]
all.mods.cases <- lapply(1:nrow(all.mods.cases), function(x) as.formula(paste("total_cases_pc~", paste(names(which(all.mods.cases[x,])), collapse="+"))))
all.mods.cases
# Deaths
model_all_var_deaths <-regsubsets(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all, nbest=10, really.big=T, intercept=F)
all.mods.deaths <- summary(model_all_var_deaths)[[1]]
all.mods.deaths <- lapply(1:nrow(all.mods.deaths), function(x) as.formula(paste("total_cases_pc~", paste(names(which(all.mods.deaths[x,])), collapse="+"))))
all.mods.deaths

# AIC analysis:
# Cases
all.lm.cases<-lapply(all.mods.cases, lm, state_var_all)
sapply(all.lm.cases, extractAIC)[2,]
min(sapply(all.lm.cases, extractAIC)[2,])
# Deaths
all.lm.deaths<-lapply(all.mods.deaths, lm, state_var_all)
sapply(all.lm.deaths, extractAIC)[2,]
min(sapply(all.lm.deaths, extractAIC)[2,])

# Model with lower AIC: 
# total_cases_pc ~ hospitals_state

# Test new model
lm_model_covid_cases_v3 <- lm((total_cases_pc) ~ sqrt(hospitals_state), data=state_var_all)
summary(lm_model_covid_cases_v3) # adj R^2: 0.1473, p-value: 0.005865

lm_model_covid_deaths_v3 <- lm(sqrt(total_deaths_pc) ~ (dem_vote_pct) + log(beds_state), data=state_var_all)
summary(lm_model_covid_deaths_v3) # adj R^2: 0.3154, p-value: 0.0003409

plot(lm_model_covid_cases_v3)
plot(lm_model_covid_deaths_v3)

# Diagnostic
# (1) Influential Observations

avPlots(lm_model_covid_cases_v3)
cutoff_cases = 4/((nrow(state_var_all)-length(lm_model_covid_cases_v3$coefficients)-2))
plot(lm_model_covid_cases_v3,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_cases_v3,main="Influence Plot for Cases",sub="circle size proportional to Cook's Distance")

avPlots(lm_model_covid_deaths_v3)
cutoff_deaths = 4/((nrow(state_var_all)-length(lm_model_covid_deaths_v3$coefficients)-2))
plot(lm_model_covid_deaths_v3,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_deaths_v3,main="Influence Plot for Deaths",sub="circle size proportional to Cook's Distance")


outlierTest(lm_model_covid_cases_v3)
leveragePlots(lm_model_covid_cases_v3)
influence.measures(lm_model_covid_cases_v3)

outlierTest(lm_model_covid_deaths_v3)
leveragePlots(lm_model_covid_deaths_v3)
influence.measures(lm_model_covid_deaths_v3)

# NOTE: Cook's Dostance reveals 6 influencial observations: 60, 148, 156, 205, 208, 312. 
# 156 and 312 are the most extreme values. We can try to remove them

# (2) Evaluate Normality
# qq plot for studentized residuals
qqPlot(lm_model_covid_cases_v3,main="QQ Plot")
qqPlot(lm_model_covid_deaths_v3,main="QQ Plot")

# Cases
sresid_cases = studres(lm_model_covid_cases_v3)
hist(sresid_cases, freq=FALSE, main="Distribution of Studentized Residuals for Cases")
xfit_cases = seq(min(sresid_cases),max(sresid_cases),length=40)
yfit_cases = dnorm(xfit_cases)
lines(xfit_cases, yfit_cases)
# Deaths
sresid_deaths = studres(lm_model_covid_deaths_v3)
hist(sresid_deaths, freq=FALSE, main="Distribution of Studentized Residuals for Deaths")
xfit_deaths = seq(min(sresid_deaths),max(sresid_deaths),length=40)
yfit_deaths = dnorm(xfit_deaths)
lines(xfit_deaths, yfit_deaths)

# (3) Evaluate homoscedasticity
# Cases
ncvTest(lm_model_covid_cases_v3)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model_covid_cases_v3)
# Deaths
ncvTest(lm_model_covid_deaths_v3)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model_covid_deaths_v3)

# (4) Evaluate Independence of errors
# DW test: null hypothesis: no autocorrelation -- fail to reject
durbinWatsonTest(lm_model_covid_cases_v3) # Suggested power transformation: 0.1435897 
durbinWatsonTest(lm_model_covid_deaths_v3) 

# Check quadratic model :
state_var_all$dem_vote_pct2 <- state_var_all$dem_vote_pct^2
state_var_all$beds_state2 <- state_var_all$beds_state^2
state_var_all$hospitals_state2 <- state_var_all$hospitals_state^2

quadratic_covid_cases_v3 <- lm(total_cases_pc ~ hospitals_state2, data=state_var_all)
summary(quadratic_covid_cases_v3) # adj R^2: 0.02554; p-value: 0.1521

quadratic_covid_deaths_v3 <- lm(total_deaths_pc ~ dem_vote_pct2 + beds_state2, data=state_var_all)
summary(quadratic_covid_deaths_v3) # adj R^2: 0.1536; p-value: 0.01235

# NOTE: R^2 dropped

# Test glm
glm_model_covid_cases_v3 <- glm(total_cases_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, 
                                data= state_var_all, family = binomial(link="logit"))
summary(glm_model_covid_cases_v3)

glm_model_covid_deaths_v3 <- glm(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, 
                                data= state_var_all, family = binomial(link="logit"))
summary(glm_model_covid_deaths_v3)

# Test beta (logit)
cases_beta <- betareg(total_cases_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
summary(cases_beta) # None significant; pseudo-R^2 of 0.1432
deaths_beta <- betareg(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
summary(deaths_beta) # dem_vote_pct and beds_state are significant

# Best models of all: deaths_beta_v2 (pseudo R^2 of 0.3153); lm_model_covid_deaths_v3 (adj R^2 of 0.3154) (both below)

#lm_model_covid_deaths_v3 <- lm(sqrt(total_deaths_pc) ~ (dem_vote_pct) + log(beds_state), data=state_var_all)
#summary(lm_model_covid_deaths_v3) # adj R^2: 0.3154, p-value: 0.0003409

#deaths_beta <- betareg(total_deaths_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
#summary(deaths_beta) # dem_vote_pct and beds_state are significant

# Best model for covid cases: cases_beta (pseudo-R^2 of 0.1432)

#cases_beta <- betareg(total_cases_pc ~ dem_vote_pct + unemploy + beds_state + hospitals_state, data=state_var_all)
#summary(cases_beta) # None significant; pseudo-R^2 of 0.1432
