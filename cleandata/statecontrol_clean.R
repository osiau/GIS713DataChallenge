### Script for cleaning state government information
### I did some cleaning outside of this script manually and converted the data file to a CSV,
### so be sure to have that file.
### This script produces a data.table with the fraction of each state's legislative
### bodies belonging to Democrats and Republicans, and the party which the
### governor of each state belongs to.

# Do also note that the final fractions may not add to 1 for each state, as there
# are independents and other parties, contrary to popular belief.

setwd("/Users/luke/Documents/NCSU/GIS713/datachallenge/Data-Challenge-GIS713-master/Datasources/") #set working directory

# define column names manually
col_names <- c("state","totalseats","totalsenate","senatedemocrats","senaterepublicans",
               "senateother","totalhouse","housedemocrats","houserepublicans","houseother",
               "legislaturecontrol","governorparty","statecontrol")

leg_control <- fread("legislative_control.csv",skip=2)

colnames(leg_control) <- col_names

# Nebraska is messed up because it has only one chamber of legislature
# Nebraska's legislature is also 'nonpartisan'
# I'm sure their legislators are all completely neutral and unaffiliated with any party
# ANYWAY, fix Nebraska manually
leg_control$totalhouse[27] = NA
leg_control$houserepublicans[27] = NA

# Now calculate the fraction of legislators belonging to the Democrats and Republicans
leg_fractions <- leg_control[,.(state = tolower(state),
                               senatefraction_democrat = senatedemocrats/totalsenate,
                               senatefraction_republican = senaterepublicans/totalsenate,
                               housefraction_democrat = housedemocrats/as.numeric(totalhouse),
                               housefraction_republican = as.numeric(houserepublicans)/as.numeric(totalhouse),
                               totalfraction_democrat = (senatedemocrats+housedemocrats)/totalseats,
                               totalfraction_republican = (senaterepublicans+as.numeric(houserepublicans))/totalseats,
                               governor = governorparty)]

leg_fractions$governor[leg_fractions$governor == "Rep"] = "republican"
leg_fractions$governor[leg_fractions$governor == "Dem"] = "democrat"
