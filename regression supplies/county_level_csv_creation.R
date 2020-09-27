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

# read the county-level COVID19 case, death and population data, and a county shapefile
data_dir <- "/Users/ihinks/Documents/GIS713/DataChallenge"
county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
# data from "USA FACTS" website: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_cases <- fread(file.path(data_dir, "covid_confirmed_usafacts.csv"))
covid_countypop <- fread(file.path(data_dir, "covid_county_population_usafacts.csv"))
covid_deaths <- fread(file.path(data_dir, "covid_deaths_usafacts.csv"))

# voting data
state_votes_all <- fread(file.path(data_dir, "statepres_1976-2016.csv"))
county_votes_all <- fread(file.path(data_dir, "countypres_2000-2016.csv"))
legislative_control <- fread(file.path(data_dir, "legislative_control.csv"))

# Get county-level votes for 2016 election
setnames(county_votes_all, "FIPS", "countyFIPS")
county_covid_cases <- merge(county_votes_all, covid_countypop, by="countyFIPS")
county_votes_2016 <- county_covid_cases[year == 2016L,]
county_votes_2016 <- county_votes_2016[, `:=`(vote_pct = candidatevotes / totalvotes, democrat = ifelse(party == "democrat", 1, 0))]

# Get county-level covid cases and death rates 
covid_deaths$total_deaths = covid_deaths[,"9/22/20"]
covid_cases$total_cases = covid_cases[,"9/19/20"]
covid_data <- merge(covid_cases[, .(countyFIPS, total_cases)], covid_deaths[, .(countyFIPS, total_deaths)], by="countyFIPS")
covid_data <- covid_data[countyFIPS > 0,]

# Output datasets: 
county_votes_cases <- merge(county_votes_2016, covid_data, by="countyFIPS")
county_votes_cases <- county_votes_cases[, `:=`(total_cases_pc = total_cases / population, total_deaths_pc = total_deaths / population)]
county_winners_2016 <- county_votes_cases[county_votes_2016[, .I[vote_pct == max(vote_pct)], by=countyFIPS]$V1] # winner by county
county_dem_votes_2016 <- county_votes_cases[democrat==1, .(countyFIPS, state_po, State, county, candidatevotes, totalvotes, population, democrat, vote_pct, total_cases_pc, total_deaths_pc)] # only dem candidate data 
# write.csv(county_dem_votes_2016, "county_dem_votes_and_cases.csv")
