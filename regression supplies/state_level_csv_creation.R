# Import packages
library(raster)
library(data.table)
library(viridis)
library(knitr)
library(conflicted)
suppressPackageStartupMessages(library(tidyverse))
conflict_prefer("filter", "dplyr")
#> [conflicted] Will prefer dplyr::filter over any other package
library(betareg)
library(broom)
library(kableExtra)
library(xlsx)

# From Josh's Rmd
# read the county-level COVID19 case, death and population data, and a county shapefile
data_dir <- "/Users/ihinks/Documents/GIS713/DataChallenge"
county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
# data from "USA FACTS" website: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_cases <- fread(file.path(data_dir, "covid_confirmed_usafacts.csv"))
covid_countypop <- fread(file.path(data_dir, "covid_county_population_usafacts.csv"))
covid_deaths <- fread(file.path(data_dir, "covid_deaths_usafacts.csv"))
state_votes_all <- fread(file.path(data_dir, "statepres_1976-2016.csv"))
county_votes_all <- fread(file.path(data_dir, "countypres_2000-2016.csv"))
legislative_control <- fread(file.path(data_dir, "legislative_control.csv"))

# Calculate cumulative covid deaths and cases by state
covid_deaths$total_deaths = covid_deaths[,"9/22/20"]
covid_cases$total_cases = covid_cases[,"9/19/20"]
covid_deaths_state <- covid_deaths[, .(total_deaths = sum(total_deaths)), by=stateFIPS]
covid_cases_state <- covid_cases[, .(total_cases = sum(total_cases)), by=stateFIPS]
covid_data_state <- merge(covid_deaths_state, covid_cases_state, by="stateFIPS")

# Calculate votes by state
setnames(county_votes_all, "FIPS", "countyFIPS")
setnames(state_votes_all, "state_fips", "stateFIPS")
state_pop <- covid_countypop[, .(population = sum(population)), by="State"]
setnames(state_pop, "State", "state_po")

# Seeing if the aggregated county-level data to the state level matches the state-level voting counts from Josh 
# (update, they're pretty close)
#state_votes_2016 <- county_votes_all[year == 2016L, .(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes)), by=.(party, state_po)]
state_votes_2016_2 <- state_votes_all[year == 2016L, .(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes)), by=.(party, stateFIPS, state_po)]

state_covid_and_votes <-merge(state_votes_2016_2, state_pop, by="state_po")

# Calculate vote percent and binary democrat (1=yes, 0=no) variables
state_covid_and_votes <- state_covid_and_votes[, `:=`(vote_pct = candidatevotes / totalvotes, democrat = ifelse(party == "democrat", 1, 0))]
# Subset to only the winner by state
state_covid_and_votes <- state_covid_and_votes[state_covid_and_votes[, .I[vote_pct == max(vote_pct)], by=stateFIPS]$V1]
state_covid_and_votes <- merge(state_covid_and_votes, covid_data_state, by="stateFIPS")
# Calculate cases and deaths per capita
state_covid_and_votes$total_cases_pc <- state_covid_and_votes$total_cases / state_covid_and_votes$population 
state_covid_and_votes$total_deaths_pc <- state_covid_and_votes$total_deaths / state_covid_and_votes$population 
# Make vote_pct positive when dem wins, negative otherwise (for regressions)
state_covid_and_votes$vote_pct <- ifelse(state_covid_and_votes$democrat == 1, state_covid_and_votes$vote_pct, -1*state_covid_and_votes$vote_pct)
# Select relevant columns
state_to_export <- state_covid_and_votes[, .(stateFIPS, population, state_po, vote_pct, democrat, total_deaths, total_cases, total_cases_pc, total_deaths_pc)]
write.csv(state_to_export, "state_votes_and_covid.csv")

