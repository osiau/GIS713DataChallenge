library(data.table)
library(RCurl)
library(httr)
library(bit64)


data_dir <- "/Users/martineelisabethmathieu/Documents/NCSU /FALL 2020/GIS 713/DATA CHALLENGE"
setwd(data_dir)

# Read data

# 1- county-level COVID19
county_covid_votes <- fread(file.path(data_dir, "county_votes_and_covid.csv"))

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


#####################################################################

# Test models

county_var_AQS_Resilience_Hospital$popn_high_risk <- as.numeric(county_var_AQS_Resilience_Hospital$popn_high_risk)
county_var_AQS_Resilience_Hospital2 <- county_var_AQS_Resilience_Hospital[, .(vote_pct, total_cases_pc, unemploy, max.aqi,
                                                                              popn_high_risk, beds_county)]

# All variables

# Scatter plots and correlations coefficents between all pairs of variables
round(cor(county_var_AQS_Resilience_Hospital2, method="pearson", use = "complete.obs"), 3)
# pairs(county_var_AQS_Resilience_Hospital)

# Test statistic for the strongest correlations
cor.test(county_var_AQS_Resilience_Hospital2$vote_pct, county_var_AQS_Resilience_Hospital2$total_cases_pc, method = "spearman")
cor.test(county_var_AQS_Resilience_Hospital2$vote_pct, county_var_AQS_Resilience_Hospital2$beds_county, method = "spearman")



library(leaps)
model_all_var<-regsubsets(vote_pct ~ total_cases_pc + unemploy + max.aqi + beds_county, 
                data=county_var_AQS_Resilience_Hospital, nbest=3, really.big=T, intercept=F)
all.mods <- summary(model_all_var)[[1]]
all.mods <- lapply(1:nrow(all.mods), function(x) as.formula(paste("vote_pct~", paste(names(which(all.mods[x,])), collapse="+"))))
all.mods

# AIC analysis:
all.lm<-lapply(all.mods, lm, county_var_AQS_Resilience_Hospital)
sapply(all.lm, extractAIC)[2,]


model_lm <- lm(vote_pct ~ total_cases_pc + unemploy + max.aqi + beds_county, data=county_var_AQS_Resilience_Hospital)
anova(model_lm )
summary(model_lm )
plot(model_lm )

# Check quadratic model with predictors from the linear model:
total_cases_pc2 <- county_var_AQS_Resilience_Hospital$total_cases_pc^2
unemploy2 <- county_var_AQS_Resilience_Hospital$unemploy^2
max.aqi2 <- county_var_AQS_Resilience_Hospital$max.aqi^2
beds_county2 <- county_var_AQS_Resilience_Hospital$beds_county^2

quadratic.model = lm(vote_pct ~ total_cases_pc + total_cases_pc2 + unemploy + unemploy2 + max.aqi + max.aqi2 + beds_county + beds_county2, data=county_var_AQS_Resilience_Hospital)
summary(quadratic.model)

# Diagnostic
# (1) Influenctal Observations
library(car)
avPlots(quadratic.model)
cutoff = 4/((nrow(county_var_AQS_Resilience_Hospital)-length(cubic.model$coefficients)-2))
plot(cubic.model,which=4,cook.levels = cutoff)
influencePlot(quadratic.model,main="Influence Plot",sub="circle size proportional to Cook's Distance")

outlierTest(quadratic.model)
leveragePlots(quadratic.model)
#influence.measures(quadratic.model)

# (2) Evaluate Normality
# qq plot for studentized residuals
qqPlot(quadratic.model,main="QQ Plot")

library(MASS)
sresid = studres(quadratic.model)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit = seq(min(sresid),max(sresid),length=40)
yfit = dnorm(xfit)
lines(xfit, yfit)

# (3) Evaluate homoscedasticity
ncvTest(quadratic.model)  
# plot studentized residuals vs. fitted values
spreadLevelPlot(quadratic.model)

# ****Need to try a log transformation

# (4) Evaluate Independence of errors
durbinWatsonTest(quadratic.model)


# Check cubic model
total_cases_pc3 <- county_var_AQS_Resilience_Hospital$total_cases_pc^3
unemploy3 <- county_var_AQS_Resilience_Hospital$unemploy^3
max.aqi3 <- county_var_AQS_Resilience_Hospital$max.aqi^3
beds_county3 <- county_var_AQS_Resilience_Hospital$beds_county^3

cubic.model = lm(vote_pct ~ total_cases_pc + total_cases_pc2 + total_cases_pc3 
                 + unemploy + unemploy2 + unemploy3 + max.aqi + max.aqi2 + max.aqi3 
                 + beds_county + beds_county2 + beds_county3, data=county_var_AQS_Resilience_Hospital)
summary(cubic.model)


