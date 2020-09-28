library(data.table)
library(RCurl)
library(httr)
library(bit64)
library(tidycensus)
library(sf)
library(tigris)
library(leaps)
library(car)
library(MASS)


data_dir <- "/Users/martineelisabethmathieu/Documents/NCSU /FALL 2020/GIS 713/DATA CHALLENGE"
setwd(data_dir)

# Read data

# 1- county-level COVID19
#county_covid_votes <- fread(file.path(data_dir, "county_votes_and_covid.csv"))

# Dependent var
county_var <- readRDS("county_dependent_vars.RDS")


###############

# 2- Air quality index : From clean data

air_qual2019 <- read.csv(file.path(data_dir,"air_quaity_annual_aqi_by_county_2019.csv"),stringsAsFactors=TRUE, header=TRUE)
air_qual2020 <- read.csv(file.path(data_dir,"air_quality_annual_aqi_by_county_2020.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- read.csv(file.path(data_dir,"all_names_FIPS.csv"),stringsAsFactors=TRUE)
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

# air_qual2020$state <- tolower(air_qual2020$state)
# air_qual2020$county <- tolower(air_qual2020$county)
# air_qual2020 <- merge(air_qual2020,data_states,by=c("state","county"))

# Completed
# Necessary variables for county 
air_qual2019_county <- air_qual2019[, .(max.aqi, median.aqi, geoID)]

# Merge
county_var_AQS <- merge(county_var, air_qual2019_county, by="geoID")

############
# 3- County resilience: From clean data
county_resilience <- fread(file.path(data_dir, "communityresilience_county.csv"), stringsAsFactors=TRUE, header=TRUE)

data_states1 <- read.csv(file.path(data_dir,"all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states1 <- as.data.table(data_states1)
data_states1 <- data_states1[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
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

# Completed
# Necessary variables for county 
county_resilience <- county_resilience[, .(geoID, popn_low_risk, popn_med_risk, popn_high_risk)]

# Merge
county_var_AQS_Resilience <- merge(county_var_AQS, county_resilience, by="geoID")


#########

# 4- Hospital: From clean data


hospitals <- fread(file.path(data_dir,"total_hospital.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states2 <- fread(file.path(data_dir,"all_names_FIPS.csv"))
data_states2 <- data_states2[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

setnames(hospitals,"T_CO_Hos(total no. of hospitals in the county)","hospitals_county")
setnames(hospitals,"T_ST_HOS(Total no. of hospitals in the States)","hospitals_state")

# removing a lot of unneeded columns

data_states2 <- unique(data_states2[,.(stateFIPS = as.factor(stateFIPS),state)])
hospitals <- hospitals[,.(geoID=as.factor(formatC(GEOID,width=5,flag="0")), stateFIPS=as.factor(formatC(STATEFP,width=2,flag="0")), 
                          countyFIPS = COUNTYF, county = tolower(NAME), beds_county = Sum_BEDS, beds_state = Sum_BEDS_1, hospitals_county, 
                          hospitals_state)]

hospitals <- merge(hospitals, data_states2, by="stateFIPS", all.x=TRUE)

# Completed
# Necessary variables for county 
county_hospitals <- hospitals[, .(geoID, beds_county, hospitals_county)]

# Merge
county_var_AQS_Resilience_Hospital <- merge(county_var_AQS_Resilience, county_hospitals, by="geoID")


#############
# 5- Census data : From clean data (with some changes)



#for ACS data 1st get a census API key ->  https://api.census.gov/data/key_signup.html
census_api_key("15044db04348948cb08ae1733e7aa0cb9b44894d", install = TRUE) #,install=TRUE only for the first time you use it 
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
# Merge
county_var_AQS_Resilience_Hospital_Census <- merge(county_var_AQS_Resilience_Hospital, acs_final, by="geoID")



#####################################################################

# Test models

county_var_AQS_Resilience_Hospital_Census$popn_low_risk <- as.numeric(county_var_AQS_Resilience_Hospital_Census$popn_low_risk)
county_var_AQS_Resilience_Hospital_Census$popn_med_risk <- as.numeric(county_var_AQS_Resilience_Hospital_Census$popn_med_risk)
county_var_AQS_Resilience_Hospital_Census$popn_high_risk <- as.numeric(county_var_AQS_Resilience_Hospital_Census$popn_high_risk)
#county_var_AQS_Resilience_Hospital2 <- county_var_AQS_Resilience_Hospital[, .(vote_pct, total_cases_pc, unemploy, max.aqi,
 #popn_high_risk, beds_county)]

# All variables

county_var_AQS_Resilience_Hospital_Census_metrics <- county_var_AQS_Resilience_Hospital_Census[,.(vote_pct, total_cases_pc, max.aqi, median.aqi, popn_low_risk, popn_med_risk, 
                                                                                                  popn_high_risk, beds_county, hospitals_county, median_age, med_income, frac_white, frac_black, 
                                                                                                  frac_asian, frac_pacislander, frac_otherrace, frac_under18, frac_over65, frac_insured, 
                                                                                                  frac_pubtransport)]
# Scatter plots and correlations coefficents between all pairs of variables
round(cor(county_var_AQS_Resilience_Hospital_Census_metrics, method="pearson", use = "complete.obs"), 3)
pairs(county_var_AQS_Resilience_Hospital_Census_metrics)

# Test statistic for the strongest correlations
#cor.test(county_var_AQS_Resilience_Hospital2$total_cases_pc, county_var_AQS_Resilience_Hospital2$vote_pct, method = "spearman")
#cor.test(county_var_AQS_Resilience_Hospital2$total_cases_pc, county_var_AQS_Resilience_Hospital2$beds_county, method = "spearman")


# Model with all predictors


lm_model_covid_cases <- lm(total_cases_pc ~ vote_pct + max.aqi + median.aqi+ popn_low_risk+ popn_med_risk+ 
                          popn_high_risk+ beds_county+ hospitals_county+ median_age+ med_income+ frac_white+ frac_black+ 
                          frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                          frac_pubtransport, data=county_var_AQS_Resilience_Hospital_Census)
 summary(lm_model_covid_cases) 
 
 # NOTE: Log transformation on total_cases_pc better
 
 
 # VIF test for multicollinearity
 # Evaluate Collinearity
 
 vif(lm_model_covid_cases)  # variance inflation factors
 sqrt(vif(lm_model_covid_cases)) > 2  # problem?
 
 # NOTE: median_age, frac_white and frac_black > 5 - Let's remove them
 
 
 # Model 2 without median_age, frac_white and frac_black
 lm_model_covid_cases_v2 <- lm(total_cases_pc ~ vote_pct + max.aqi + median.aqi+ popn_low_risk+ popn_med_risk+ 
                              popn_high_risk+ beds_county+ hospitals_county+ med_income+   
                              frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                              frac_pubtransport, data=county_var_AQS_Resilience_Hospital_Census)
 summary(lm_model_covid_cases_v2) 
 
 
 

# AIC test


model_all_var<-regsubsets(total_cases_pc ~ vote_pct + max.aqi + median.aqi+ popn_low_risk+ popn_med_risk+ 
                          popn_high_risk+ beds_county+ hospitals_county+ med_income+   
                          frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                          frac_pubtransport, data=county_var_AQS_Resilience_Hospital_Census, nbest=10, really.big=T, intercept=F)
all.mods <- summary(model_all_var)[[1]]
all.mods <- lapply(1:nrow(all.mods), function(x) as.formula(paste("total_cases_pc~", paste(names(which(all.mods[x,])), collapse="+"))))
all.mods

# AIC analysis:
all.lm<-lapply(all.mods, lm, county_var_AQS_Resilience_Hospital_Census)
sapply(all.lm, extractAIC)[2,]

# Model with lower AIC: 
#total_cases_pc ~ vote_pct + hospitals_county + med_income + frac_pacislander + frac_otherrace + frac_under18 + frac_over65 + frac_insured

# Test new model
lm_model_covid_cases_v3 <- lm(total_cases_pc ~ vote_pct + hospitals_county + med_income + frac_pacislander + 
                                frac_otherrace + frac_under18 + frac_over65 + frac_insured, 
                              data= county_var_AQS_Resilience_Hospital_Census)

summary(lm_model_covid_cases_v3)

plot(lm_model_covid_cases_v3)


# Diagnostic
# (1) Influenctal Observations

avPlots(lm_model_covid_cases_v3)
cutoff = 4/((nrow(county_var_AQS_Resilience_Hospital_Census)-length(lm_model_covid_cases_v3$coefficients)-2))
plot(lm_model_covid_cases_v3,which=4,cook.levels = cutoff)
influencePlot(lm_model_covid_cases_v3,main="Influence Plot",sub="circle size proportional to Cook's Distance")

outlierTest(lm_model_covid_cases_v3)
leveragePlots(lm_model_covid_cases_v3)
influence.measures(lm_model_covid_cases_v3)

# NOTE: Cook's Dostance reveals 6 influencial observations: 60, 148, 156, 205, 208, 312. 
# 156 and 312 are the most extreme values. We can try to remove them

# (2) Evaluate Normality
# qq plot for studentized residuals
qqPlot(lm_model_covid_cases_v3,main="QQ Plot")


sresid = studres(lm_model_covid_cases_v3)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit = seq(min(sresid),max(sresid),length=40)
yfit = dnorm(xfit)
lines(xfit, yfit)

# (3) Evaluate homoscedasticity
ncvTest(lm_model_covid_cases_v3)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(lm_model_covid_cases_v3)

# NOTE: Suggested power transformation:  0.5334094 

# (4) Evaluate Independence of errors
durbinWatsonTest(lm_model_covid_cases_v3)




# Check quadratic model :
vote_pct2 <- county_var_AQS_Resilience_Hospital_Census$vote_pct^2
hospitals_county2 <- county_var_AQS_Resilience_Hospital_Census$hospitals_county^2
med_income2 <- county_var_AQS_Resilience_Hospital_Census$med_income^2
frac_pacislander2 <- county_var_AQS_Resilience_Hospital_Census$frac_pacislander^2
frac_otherrace2 <- county_var_AQS_Resilience_Hospital_Census$frac_otherrace^2
frac_under182 <- county_var_AQS_Resilience_Hospital_Census$frac_under18^2
frac_over652 <- county_var_AQS_Resilience_Hospital_Census$frac_over65^2
frac_insured2 <- county_var_AQS_Resilience_Hospital_Census$frac_insured^2
  


quadratic_covid_cases_v3 <- lm(total_cases_pc ~ vote_pct2 + hospitals_county2 + med_income2 + frac_pacislander2 + 
                                 frac_otherrace2 + frac_under182 + frac_over652 + frac_insured2, 
                               data= county_var_AQS_Resilience_Hospital_Census)

summary(quadratic_covid_cases_v3)

# NOTE: R^2 dropped

# Test glm
glm_model_covid_cases_v3 <- glm(total_cases_pc ~ vote_pct + hospitals_county + med_income + frac_pacislander + 
                                  frac_otherrace + frac_under18 + frac_over65 + frac_insured, 
                                data= county_var_AQS_Resilience_Hospital_Census, family = binomial(link="logit"))
summary(glm_model_covid_cases_v3)


