# Import packages
library(raster)
library(data.table)
library(viridis)
library(knitr)
library(conflicted)
suppressPackageStartupMessages(library(tidyverse))
conflict_prefer("filter", "dplyr")
library(betareg)
library(broom)
library(kableExtra)

# From https://hansjoerg.me/2019/05/10/regression-modeling-with-proportion-data-part-1/
# Function to transform attendance rates for beta regression because values of 0
# and/or 1 may occur
transform_perc <- function(percentage_vec) {
  # See Cribari-Neto & Zeileis (2010)
  (percentage_vec * (length(percentage_vec) - 1) + 0.5)/length(percentage_vec)
}

# Function to calculate a and b parameter of beta distribution from mu and phi
# used in betareg with link function logit for mu and log for phi
calc_ab <- function(mu, phi) {
  mu <- plogis(mu)
  phi <- exp(phi)
  b <- phi - mu*phi
  a <- phi - b
  return(cbind(a = a, b = b)[, 1:2])
}

# From Josh's Rmd
# read the county-level COVID19 case, death and population data, and a county shapefile
data_dir <- "/Users/ihinks/Documents/GIS713/DataChallenge"
county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
# data from "USA FACTS" website: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_cases <- fread(file.path(data_dir, "covid_confirmed_usafacts.csv"))
covid_countypop <- fread(file.path(data_dir, "covid_county_population_usafacts.csv"))
covid_deaths <- fread(file.path(data_dir, "covid_deaths_usafacts.csv"))
# voting data
state_votes_all <- fread(file.path(data_dir, "statepres_1976-2016.csv"))

# join the county population data
covid_cases <- merge(covid_countypop[, .(countyFIPS, population)], covid_cases, by="countyFIPS")

# fix the names to be R-compliant
names(covid_cases)
names(covid_cases)[3] <- "countyName" # get rid of space in var name
# change case count variable names to be dYYYYJJJ format
covid_dates <- as.Date(names(covid_cases)[6:ncol(covid_cases)], format="%m/%d/%y")
names(covid_cases)[6:ncol(covid_cases)] <- strftime(covid_dates, format="d%Y%j")
covid_date_cols <- c(strftime(covid_dates, format="d%Y%j"))
#covid_cases_2 <- covid_cases[, total_cases := rowSums(.SD), .SDcols = 6:ncol(covid_cases)]

# Get state-level data
covid_cases[countyName == "Statewide Unallocated", .(countyName, State, d2020259)]

# Adding state-level population data 
state_pops <- covid_countypop[, .(state_pop = sum(population)), by=State] # aggregate to state level
covid_cases <- covid_cases[state_pops, on='State', population := i.state_pop]
rm(state_pops)

# Per capita cases across all observation dates
case_cols <- names(covid_cases)[6:ncol(covid_cases)]
new_names <- paste(case_cols, "pc", sep="_")
covid_cases[, (new_names) := lapply(.SD, function(cases) cases / population), .SDcols=case_cols]

# Get total covid cases per capita (cumulative data)
covid_cases[, total_pc := d2020263_pc]

# Subset to 2016 data 
state_votes_2016 <- state_votes_all[year == 2016L,] # Subset to 2016 data
setnames(state_votes_2016, "state_fips", "stateFIPS") # Rename for same FIPS variable name

# Add total per capita covid cases to state-level voting data
state_votes_2016[covid_cases, on='stateFIPS', total_cases_pc := i.total_pc]

# Create cols for vote percent and binary 1/0 for dem/not dem
state_votes_2016 <- state_votes_2016[, `:=`(vote_pct = candidatevotes / totalvotes, democrat = ifelse(party == "democrat", 1, 0))]

# Generate exploratory data tables
state_winners_2016 <- state_votes_2016[state_votes_2016[, .I[vote_pct == max(vote_pct)], by=stateFIPS]$V1] # only the winner per state
state_votes_2016 <- state_votes_2016[, .(total_cases_pc=total_cases_pc, vote_pct = sum(vote_pct)), by=.(state, state_po, stateFIPS, democrat)] # aggregated to dem/non-dem votes per state
state_dem_cases <- state_votes_2016[democrat==1] # only dem candidate data 

# *** Models start here **********************************************************************************************************************************
# ___ Winning candidate by state _______________________________________________________________________________________
# binomial regression looking at winner in each state
state_winners_logit <- glm(democrat ~ total_cases_pc, data=state_winners_2016, family = "binomial")
summary(state_winners_logit)
boxplot(total_cases_pc~democrat, data=state_winners_2016)
# Removing outliers
dem_cases_stats <- boxplot.stats(state_winners_2016[democrat == 1]$total_cases_pc, coef=2)
other_cases_stats <- boxplot.stats(state_winners_2016[democrat == 0]$total_cases_pc, coef=2)
# Winning candidates with outlier obs removed 
state_winners_cln <- state_winners_2016[!(total_cases_pc %in% c(dem_cases_stats$out, other_cases_stats$out)),]
state_winners_logit <- glm(democrat ~ total_cases_pc, data=state_winners_cln, family = "binomial")
summary(state_winners_logit)
boxplot(total_cases_pc~democrat, data=state_winners_cln)

# ___ % votes for dems by state ________________________________________________________________________________________
# Explore correlation between % votes for dems and covid cases 
hist(state_dem_cases$vote_pct) # normally distributed 
hist(state_dem_cases$total_cases_pc) # indicates significant outlier: Washington DC
state_dem_cases[which.max(state_dem_cases$total_cases_pc),] # Washington DC 
# Removing outliers
dem_case_stats <- boxplot.stats(state_dem_cases$total_cases_pc, coef=2)
state_dem_cases_cln <- state_dem_cases[!(total_cases_pc %in% dem_case_stats$out)]
# Ask team what I should do with the case for Hillary as a write-in candidate 
#state_dem_cases_noDC <- state_dem_cases[state != "District of Columbia"]
hist(log(state_dem_cases_cln$total_cases_pc))
scatter.smooth(state_dem_cases_cln$vote_pct, log(state_dem_cases_cln$total_cases_pc), main = "Cases per capita by % Votes for dems")

dem_mods <- list()

# Linear model w/ log transformation on covid cases
dem_mods$lm <- lm(log(total_cases_pc) ~ vote_pct, data=state_dem_cases_cln)

# Quasi-binomial 
dem_mods$bin <- glm(total_cases_pc ~ vote_pct, data=state_dem_cases_cln, family = quasibinomial(logit))

# Beta regression (logit) (as per https://hansjoerg.me/2019/05/10/regression-modeling-with-proportion-data-part-1/)
dem_mods$beta <- betareg(total_cases_pc ~ vote_pct, data=state_dem_cases_cln)

# Beta (loglog)
dem_mods$loglog <- update(dem_mods$beta, link="loglog")

