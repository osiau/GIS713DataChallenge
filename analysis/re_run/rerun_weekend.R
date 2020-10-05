library(bbmle)
library(RCurl)
library(httr)
library(bit64)
library(sf)
library(tigris)
library(MASS)
library(raster)
library(car)
library(data.table)
library(leaps)
library(spdep)
library(stats)
library(reghelper)

wd <- getwd()

# Read data
county_indep <- readRDS(file.path(wd,"Datasources/county_independent_vars.RDS"))
county_depend <- readRDS(file.path(wd,"Datasources/county_dependent_vars.RDS"))

nrow(county_indep)
nrow(county_depend)
full_data <- merge(county_indep, county_depend,by="geoID",all.y=TRUE)
full_data <- as.data.table(full_data)

# Data formatting
# convert vote_pct to percent Clinton votes
full_data$vote_pct <- ifelse(full_data$vote_pct<0,
                               (1-(full_data$vote_pct*-1)),full_data$vote_pct)


full_data$res_high_risk <- as.numeric(full_data$res_high_risk)
full_data$res_med_risk <- as.numeric(full_data$res_med_risk)
full_data <- full_data[!is.na(median_age)] # removing non-census track counties (PR)
names(full_data)

# Running some models!
######## 1- Covid cases, deaths, and unemployment
lm_model_covid_cases <- lm(total_cases_pc ~ vote_pct + aqi_2019 + res_med_risk + frac_amindian +
                             res_high_risk+ beds_county+ hospitals_county+ median_age+ med_income+ frac_white+ frac_black+ 
                             frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                             frac_pubtransport, data=full_data)

lm_model_covid_deaths <- lm(total_deaths_pc ~ vote_pct + aqi_2019 + res_med_risk + frac_amindian +
                             res_high_risk+ beds_county+ hospitals_county+ median_age+ med_income+ frac_white+ frac_black+ 
                             frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                             frac_pubtransport, data=full_data)

lm_model_unemploy <- lm(unemploy ~ vote_pct + aqi_2019 + res_med_risk + frac_amindian +
                             res_high_risk+ beds_county+ hospitals_county+ median_age+ med_income+ frac_white+ frac_black+ 
                             frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_over65+ frac_insured+ 
                             frac_pubtransport, data=full_data)

# VIF test for multicollinearity

vif(lm_model_covid_cases)  # variance inflation factors
vif(lm_model_covid_cases) > 4  #consider removing variables with VIFs >4

vif(lm_model_covid_deaths)  # variance inflation factors
vif(lm_model_covid_deaths) > 4  #consider removing variables with VIFs >4

vif(lm_model_unemploy)  # variance inflation factors
vif(lm_model_unemploy) > 4  #consider removing variables with VIFs >4

# NOTE: median_age, frac_white, frac_black and frac_over65 > 5; 
# We should only remove 1 of the pairs that are collinear
# KEEP frac_black and frac_over65 
# AGREED, but also this means we can assume that the direction of the relationship for the 
# removed collinear variable is the opposite based on correlogram

lm_model_covid_cases_v2 <- lm(total_cases_pc ~ vote_pct + aqi_2019+  + frac_black + res_med_risk+ frac_amindian + frac_asian +
                                res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                                frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                                frac_pubtransport, data=full_data) #Now we are cooking :)!

lm_model_covid_deaths_v2 <- lm(total_deaths_pc ~ vote_pct + aqi_2019+  + frac_black + res_med_risk+ frac_amindian + frac_asian +
                                 res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                                 frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                                 frac_pubtransport, data=full_data)

lm_model_covid_unemploy_v2 <- lm(unemploy ~ vote_pct + aqi_2019+  + frac_black + res_med_risk+ frac_amindian + frac_asian +
                                   res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                                   frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                                   frac_pubtransport, data=full_data)

vif(lm_model_covid_cases_v2)  # variance inflation factors
vif(lm_model_covid_cases_v2) > 4  # problem? nope, we are good!


# AIC test - ON CASES

model_all_var<-regsubsets(total_cases_pc ~ vote_pct + aqi_2019+ frac_black + res_med_risk+ frac_amindian + frac_asian +
                            res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                            frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                            frac_pubtransport, data=full_data, nbest=10, really.big=T, intercept=F)

all.mods <- summary(model_all_var)[[1]]
all.mods <- lapply(1:nrow(all.mods), function(x) as.formula(paste("total_cases_pc~", paste(names(which(all.mods[x,])), collapse="+"))))

# AIC analysis:
all.lm <-lapply(all.mods, lm, full_data)
#sapply(all.lm, extractAIC)[2,]

#Alternate AIC analysis with bblme package:
AICtab(all.lm, weights=T, logLik=T) #model 78 has the most support (weight = 0.77), but model 68 (weight within 0.23) is within 2.5 delta AIC, so we may want to consider it as well.

#Model with lower AIC -72-: 
# frac_black, med_income, frac_over65, vote_pct, frac_otherrace,  frac_under18,  frac_insured, frac_amindian  
names(all.lm[[77]]$coefficients)[-1]

# Test new models

lm_model_covid_cases_v3 <- lm(total_cases_pc ~ frac_black + med_income + vote_pct  +
                                 frac_over65  + frac_amindian + frac_otherrace + frac_insured +
                                 frac_under18, data= full_data)

summary(lm_model_covid_cases_v3)

# ON DEATHS

model_all_var<-regsubsets(total_deaths_pc ~ vote_pct + aqi_2019+ frac_black + res_med_risk+ frac_amindian + frac_asian +
                            res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                            frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                            frac_pubtransport, data=full_data, nbest=10, really.big=T, intercept=F)

all.mods <- summary(model_all_var)[[1]]
all.mods <- lapply(1:nrow(all.mods), function(x) as.formula(paste("total_deaths_pc~", paste(names(which(all.mods[x,])), collapse="+"))))

# AIC analysis:
all.lm <-lapply(all.mods, lm, full_data)
#sapply(all.lm, extractAIC)[2,]

#Alternate AIC analysis with bblme package:
AICtab(all.lm, weights=T, logLik=T) #model 78 has the most support (weight = 0.77), but model 68 (weight within 0.23) is within 2.5 delta AIC, so we may want to consider it as well.

#Model with lower AIC -72-: 66, 76
# frac_black, med_income, frac_over65, frac_pacislander, frac_otherrace,  frac_under18,  frac_pubtransport, frac_amindian  
names(all.lm[[76]]$coefficients)[-1]
names(all.lm[[66]]$coefficients)[-1]

# Test new models

lm_model_covid_deaths_v3 <- lm(total_deaths_pc ~ frac_black + med_income + frac_amindian +
                                frac_over65  + frac_pacislander + frac_otherrace +
                                frac_pubtransport + frac_under18, data= full_data)

summary(lm_model_covid_deaths_v3)

# UNEMPLOYMENT

model_all_var<-regsubsets(unemploy ~ vote_pct + aqi_2019+ frac_black + res_med_risk+ frac_amindian + frac_asian +
                            res_high_risk+ beds_county+ hospitals_county+ med_income+ frac_over65+   
                            frac_asian+ frac_pacislander+ frac_otherrace+ frac_under18+ frac_insured+ 
                            frac_pubtransport, data=full_data, nbest=10, really.big=T, intercept=F)

all.mods <- summary(model_all_var)[[1]]
all.mods <- lapply(1:nrow(all.mods), function(x) as.formula(paste("total_deaths_pc~", paste(names(which(all.mods[x,])), collapse="+"))))

# AIC analysis:
all.lm <-lapply(all.mods, lm, full_data)
#sapply(all.lm, extractAIC)[2,]

#Alternate AIC analysis with bblme package:
AICtab(all.lm, weights=T, logLik=T) #model 78 has the most support (weight = 0.77), but model 68 (weight within 0.23) is within 2.5 delta AIC, so we may want to consider it as well.

# top model 73

names(all.lm[[73]]$coefficients)[-1]

lm_model_unemploy_v3 <- lm(total_deaths_pc ~ frac_black + med_income + 
                                 frac_over65  + frac_pacislander + frac_otherrace +
                                 frac_pubtransport + frac_insured + frac_under18, data= full_data)

summary(lm_model_unemploy_v3)
