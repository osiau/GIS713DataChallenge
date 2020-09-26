library(data.table)
library(sp)
library(tidycensus)


#for ACS data 1st get a census API key ->  https://api.census.gov/data/key_signup.html
census_api_key("dd4d4e55f2697156b23114e25c0d869d0e04a368") #,install=TRUE only for the first time you use it 
# First time, reload your environment so you can use the key without restarting R.
#readRenviron("~/.Renviron")
# You can check it with:
#Sys.getenv("CENSUS_API_KEY")

#to find ACS table variable ids https://walker-data.com/tidycensus/articles/basic-usage.html
#example on get_acs
blue_states <- c("MA", "CA", "VT") 

factors <- c(
  total_pop = "B01003_001",
  pub_transport = "B08101_025"
  )

tmp <- get_acs(
  geography = "state",
  variables = factors,
  state = blue_states,
  year = 2018,
  survey = "acs5",
  geometry = TRUE
  )

tmp

#"B08101_009" car/truck/van carpool , "B08101_017" car/truck/van alone , "B08101_025" public transportation no cab ,"B08101_041" taxicab/motorcycle/bike/other ,"B08101_041" walked


#for all the other stuff
setwd("Datasources") #set working directory 

csvtables <- list.files(pattern="*.csv")
tmp <- lapply(csvtables, fread)

csvtables #all csvs
confirmedcovid <- tmp[[7]] #access one datatable

county_data <- shapefile("cb_2015_us_county_20m/cb_2015_us_county_20m.prj") 
