### Script for cleaning state-level comorbidity data

## These data are slightly confusing, so anyone working with this may want to
## check the notes and footnotes here: https://data.cdc.gov/NCHS/Conditions-contributing-to-deaths-involving-corona/hk9y-quqm
## In particular, these data refer to the comorbidities which people who died
## from COVID-19 have. So, people can have multiple comorbidities, and they
## are counted under each of them.

# The data are kept in long format here.

# The data also have inconsistencies; sometimes counts with less than 10 are suppressed
# (i.e. missing), but some also seem to be present. I'll just assume the missing
# values are 0.

library(data.table)
library(cdlTools) # for obtaining FIPS codes
library(stringr)

#setwd("/Users/luke/Documents/NCSU/GIS713/datachallenge/Data-Challenge-GIS713-master/Datasources/") #set working directory
fileloc <- "comorbiditiesbyage.csv"

comorbidity_table <- fread(fileloc)

# This also makes a data table for the total national deaths associated with each
# comorbidity, for possible reference (though I assume these totals include territories)
comorbidity_all_national <- comorbidity_table[State=="US" & `Age Group` == "All ages"]

states_to_exclude <- c("US","PR","YC") # exclude territories and national totals
comorbidity_clean <- comorbidity_table[State %in% states_to_exclude == 0,
                                       .(state=tolower(State), stateFIPS=str_pad(fips(State),2,"left","0"),
                                         condition_group=`Condition Group`,
                                         condition=Condition, age_group=`Age Group`,
                                         deaths_with_comorbidity=`Number of COVID-19 Deaths`)]
comorbidity_clean$deaths_with_comorbidity[is.na(comorbidity_clean$deaths_with_comorbidity)] = 0

# Also make columns with percentages of COVID deaths associated with each comorbidity
comorbidity_covidtotals <- comorbidity_clean[condition == "COVID-19"]
comorbidity_clean$deaths <- 0 # create a new column for these totals
for (n in 1:nrow(comorbidity_covidtotals)) {
  comorbidity_clean$deaths[comorbidity_clean$state == comorbidity_covidtotals$state[n] &
                             comorbidity_clean$age_group == comorbidity_covidtotals$age_group[n]] = 
    comorbidity_covidtotals$deaths_with_comorbidity[n]
}
comorbidity_clean <- comorbidity_clean[,pct_with_comorbidity:=deaths_with_comorbidity/deaths]
# lots of dividing by 0 happens here
# there are also some cases with fractions greater than 1

# and this makes another table, without the COVID total deaths
# might be useful for quickly comparing comorbidities themselves
comorbidity_clean_nocovidtotals <- comorbidity_clean[condition != "COVID-19"]