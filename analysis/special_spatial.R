library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
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
library(spgwr)

# FROM RE-RUN, data:

wd <- getwd()

# Read data
county_indep <- readRDS(file.path(wd,"Datasources/county_independent_vars.RDS"))
county_depend <- readRDS(file.path(wd,"Datasources/county_dependent_vars.RDS"))

nrow(county_indep)
nrow(county_depend)
full_data <- merge(county_indep, county_depend,by="geoID",all.x=TRUE)

#####################################################################
# Data formatting
# convert vote_pct to percent Clinton votes
full_data$vote_pct <- ifelse(full_data$vote_pct<0,
                             (1-(full_data$vote_pct*-1)),full_data$vote_pct)

full_data$res_high_risk <- as.numeric(full_data$res_high_risk)
full_data$res_med_risk <- as.numeric(full_data$res_med_risk)
full_data <- full_data[!is.na(median_age)] # removing non-census track counties (PR)


# AS REFERENCE: these were the models that came out of the first AIC
# I was not feeling brave enough to attempt any transformations or error dist changes 
# BUT PLEASE FEEL FREE!

lm_model_covid_cases_v3 <- lm(total_cases_pc ~ frac_black + med_income + vote_pct  +
                                frac_over65  + frac_amindian + frac_otherrace + frac_insured +
                                frac_under18, data= full_data)

lm_model_covid_deaths_v3 <- lm(total_deaths_pc ~ frac_black + med_income + frac_amindian +
                                 frac_over65  + frac_pacislander + frac_otherrace +
                                 frac_pubtransport + frac_under18, data= full_data)

lm_model_unemploy_v3 <- lm(unemploy ~ frac_black + med_income + 
                             frac_over65  + frac_pacislander + frac_otherrace +
                             frac_pubtransport + frac_insured + frac_under18, data= full_data)

# SHAPEZ

county_shp <- shapefile(file.path(wd,"Datasources/cb_2015_us_county_20m.shp"))
setnames(county_shp@data,"GEOID","geoID")

# To make the spatial auto-corr more manageable, I subset to just the key columns in order
# to remove any NAs that seemed to be causing problems in Moran's I

full_cases <- full_data[!is.na(total_cases_pc),.(geoID,total_cases_pc, frac_black,med_income,vote_pct,
                                                 frac_over65,frac_amindian,frac_otherrace,
                                                 frac_insured,frac_under18)]

full_deaths <- full_data[!is.na(total_deaths_pc),.(geoID,total_deaths_pc, frac_black,med_income, frac_amindian,
                                                  frac_over65,frac_pacislander,frac_otherrace,
                                                  frac_pubtransport,frac_under18)]
  

full_unemploy <- full_data[!is.na(unemploy),.(geoID,unemploy, frac_black,med_income,
                                                    frac_over65,frac_pacislander,frac_otherrace,
                                                    frac_pubtransport,frac_insured,frac_under18)]

full_cases <- full_cases[!is.na(med_income)] # needed to remove 2 NA values
full_deaths <- full_deaths[!is.na(med_income)] # needed to remove 2 NA values
full_unemploy <- full_unemploy[!is.na(med_income)] # needed to remove 2 NA values

# Merge to the dimensions of the data (not the shapefile)
county_cases <- merge(county_shp, full_cases, by="geoID", all.y=TRUE, all.x=FALSE)
county_deaths <- merge(county_shp, full_deaths, by="geoID", all.y=TRUE, all.x=FALSE)
county_unemploy <- merge(county_shp, full_unemploy, by="geoID", all.y=TRUE, all.x=FALSE)

# Running spatial autocorrelation:

# Cases

model <- lm_model_covid_cases_v3

# Prepare the weighted matrix!

w <- 1 / as.matrix(dist(coordinates(county_cases)))
diag(w) <- 0

# moran.test(full_cases$total_cases_pc, mat2listw(w))
# moran.test(residuals(model), mat2listw(w))

# Now we're reeeeeally cookin'

lm.morantest(model, mat2listw(w),naSubset=FALSE)

county_cases$cases_lm_resids <- model$residuals

# plotting contiguous states

county_cases <- county_cases[!county_cases$STATEFP %in% c("02", "15", "72"),]
spplot(county_cases, "cases_lm_resids",main="Residuals of LM for COVID Cases")

# Deaths

model <- lm_model_covid_deaths_v3
# model <- lm_model_covid_unemploy_v3

# Prepare the weighted matrix!

w <- 1 / as.matrix(dist(coordinates(county_deaths)))
diag(w) <- 0

# Now we're reeeeeally cookin'

lm.morantest(model, mat2listw(w),naSubset=FALSE)

county_deaths$deaths_lm_resids <- model$residuals

# plotting contiguous states

county_deaths <- county_deaths[!county_deaths$STATEFP %in% c("02", "15", "72"),]
spplot(county_deaths, "deaths_lm_resids",main="Residuals of LM for COVID Deaths")

# Unemployment

model <- lm_model_unemploy_v3

# Prepare the weighted matrix!

w <- 1 / as.matrix(dist(coordinates(county_unemploy)))
diag(w) <- 0

# Now we're reeeeeally cookin'

lm.morantest(model, mat2listw(w),naSubset=FALSE)
county_unemploy$unemploy_lm_resids <- model$residuals

# plotting contiguous states

county_unemploy <- county_unemploy[!county_unemploy$STATEFP %in% c("02", "15", "72"),]
spplot(county_unemploy, "unemploy_lm_resids",main="Residuals of LM for Unemployment")