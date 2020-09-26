### Script for reading in relevant county-level ACS data for 2018 (most recent year available)
### and cleaning the data to produce a data.table with county names, IDs, geometries,
### and demographic data normalized by the estimated population.

library(data.table)
library(tidycensus)

#for ACS data 1st get a census API key ->  https://api.census.gov/data/key_signup.html
census_api_key("0701d8aec5d95c9a8b2c0f3ba8067ace6de1e764") #,install=TRUE only for the first time you use it 
# First time, reload your environment so you can use the key without restarting R.
#readRenviron("~/.Renviron")
# You can check it with:
#Sys.getenv("CENSUS_API_KEY")

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
  geometry = TRUE
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
    acs_wide$geometry <- temp_singlevar$geometry
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
acs_final <- acs_wide[,.(geoid,county,state,geometry,median_age,med_income,frac_white,
                               frac_black,frac_amindian,frac_asian,frac_pacislander,
                               frac_otherrace,frac_under18,frac_over65,frac_insured,
                         frac_pubtransport)]
