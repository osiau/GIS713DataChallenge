###############################################################################
# Script to create .csv file of total counts and total deaths of Covid19
# before the re-opening date and after the reopening date (preSum and postSum)
#
# For Dr. Joshua Gray & NCSU 48 Hour Data Challenge
#
# @ data -> https://aws.amazon.com/marketplace/pp/COVID-19-United-States-Reopen-and-Shut-Down-Status/prodview-ejbvrkmiwc5so#overview
# @ data -> covid_confirmed_usafacts.csv
# @ data -> covid_deaths_usafacts.csv
###############################################################################
# Import Libraries
library(data.table)
library(svMisc)

# Set data files (only deats and counts available for Covid19)
# reopen status file used to identify re-opening date
setwd("/home/nick/Downloads")
data <- fread("covid_confirmed_usafacts.csv")
#data <- fread("covid_deaths_usafacts.csv")
reopens <- fread("nyt-states-reopen-status-covid-19_dataset_nyt-states-reopen-status-covid-19.csv")

# Because re-open dates are strings with non regex characters they were corrected and split
reopens$date_details <- gsub("Reopened", "", reopens$date_details, ignore.case = TRUE)
reopens$date_details <- substring(reopens$date_details, 1, nchar(reopens$date_details)-1)
reopens$date_details <- strsplit(reopens$date_details, " ")

# Vectors to store the month, day, and aggregated date were created
month <- rep(0, length(reopens$date_details))
day <- rep(0, length(reopens$date_details))
date <- rep(0, length(reopens$date_details))

# Iterate the regex string corrected reopens data 
for(i in 1 : length(reopens$date_details))
{
  # Access last two indexes of the list of string values
  reopens$date_details[[i]] <- reopens$date_details[[i]][c(length(reopens$date_details[[i]])-1, length(reopens$date_details[[i]]))]
  
  # Conditionals to determine numeric value based on Month string
  if(reopens$date_details[[i]][1] == "April")
  {
    month[i] = 4
  }
  if(reopens$date_details[[i]][1] == "May")
  {
    month[i] = 5
  }
  if(reopens$date_details[[i]][1] == "June")
  {
    month[i] = 6
  }
  
  # Set day as numeric from numeric string in date_details data
  day[i] <- as.numeric(as.character(reopens$date_details[[i]][2]))
  
  # Paste values together to form a date variable equivielent to Covid data columns' format
  date[i] <- paste0(month[i], "/", day[i], "/", "20")

}

# Set the date_details data as the newly formated date
reopens$date_details <- date

# Create empty vectors to store the state, county, before re-opening, and post re-opening values
fips <- vector(length=length(data$'County Name'))
state <- vector(length=length(data$'County Name'))
county <- vector(length=length(data$'County Name'))
preSum <- vector(length=length(data$'County Name'))
postSum <- vector(length=length(data$'County Name'))
x <- 0
# Iterate covid data and set each empty vector equal to state, county, preSum, and postSum
for(i in 1:length(data$countyFIPS))
{
  # Iterate columns in data and reopen dates to identify the index where the reopen was
  for(j in 5:length(names(data)))
  {
    for(k in 1:length(reopens$date_details))
    {
      if(names(data)[j] == reopens$date_details[k])
      {
        if(data$State[i] == reopens$state_abbreviation[k])
        {
          x <- j 
          #reopenCol <- names(data)[x]
        }
        
      }
    }
  }
  
  fips[i] <- data$countyFIPS[i]
  state[i] <- data$State[i]
  county[i] <- data$'County Name'[i]
  
  # sum covid counts from first date to reopen date
  #preSum[i] <- rowSums(data[i, 5:x])
  
  # sum covid counts from reopen date to last date
  #postSum[i] <- rowSums(data[i, x:length(names(data))])
  
  # Get total count value for day of re-opening
  preSum[i] <- data[[names(data)[x]]][i]
  
  # Get totla count value for last day of counts
  postSum[i] <- data[[names(data)[length(names(data))]]][i]
  
  # print progress
  progress(i)
}

# Create a data.frame and save data
data <- data.frame(fips, state, county, preSum, postSum)
setwd("/home/nick/Downloads")
write.csv(data, "reOpeningCountsRvsd.csv")
