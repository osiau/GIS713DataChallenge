library(data.table)
library(stringr)
library(rgdal)
library(tidyr)
library(tmap) #for viewing only
library(rgeos)

#Import shapefile. "..." is the shortcut on my computer via Googel Stream Drive or whatever it's called.
sp1 <- readOGR(".../datachallenge/Datasources/cb_2015_us_county_20m", "cb_2015_us_county_20m")

#Drop Puerto Rico
sp2 <- sp1[sp1$STATEFP!="72",]

#Read in county vote counts
co_pres16 <- fread("https://github.ncsu.edu/chaedri/Data-Challenge-GIS713/blob/master/cleandata/county_pres_2016.csv")

#Fix FIPS part I
co_pres$FIPS <- as.character(co_pres$FIPS)

#Subset for 2016
co_pres16 <- co_pres[ party == "democrat" | party == "republican"),]

#Recast from long to wide to have candidate counts/percentages on one row. Can't merge with spatial object otherwise.				  
co_pres16 <- dcast(co_pres16, year+state+state_po+county+FIPS+office+totalvotes~candidate, value.var="candidatevotes")

#Necessary for next step
setnames(co_pres16,  "Hillary Clinton" ,"Hillary_Clinton")

#Calculate percentage of votes for democrat
co_pres16[, `:=` (percent_dem = Hillary_Clinton / totalvotes)]

#Calculate percentage of votes for republican
co_pres16[, percent_rep := 1 - percent_dem]

#Binary value of democrat win
co_pres16[, dem_win := ifelse(percent_dem > .50, 1, 0)]

#Drop rows that won't merge with SPDF
pres_2016_co <- co_pres16[which(!is.na(co_pres16$state_po)),]

#Fix FIPS part II.
pres_2016_co$FIPS[str_length(pres_2016_co$FIPS) < 5] <- paste0("0", pres_2016_co$FIPS[(str_length(pres_2016_co$FIPS) < 5)])

#Merge county votes DT with SPDF
pres_2016_co_map <- merge(sp2, pres_2016_co, by.x="GEOID", by.y="FIPS", all.x = T)

tm_shape(pres_2016_co_map) + tm_fill("percent_dem")

#Import Alaska map of voting districts, which are not the same as counties when it comes to vote counts. "---"is a local directory 
#for me, but this file will be added to Google or Github soone.
alp1 <- readOGR("---/2013-HD-ProclamationPlan", "2013ProclamationPlan")

mepg <- make_EPSG()

#Select for Albers Equal Area for presentation
alberse <- mepg[mepg$code == "5070","prj4"]

#Subset to Alaska votes only
ak_votes <- pres_2016_co[pres_2016_co$state=="Alaska",]

#Edit for district numbers only
ak_votes$district_no <- gsub("District ","", ak_votes$county)

#Transform
pres_2016_co_map <- spTransform(pres_2016_co_map, alberse)

#Transform
ak_vote_map <- spTransform(alp1, alberse)

#Merge vote information with SPDF
ak_map_votes <- merge(ak_vote_map, ak_votes, by.x="District_N", by.y="district_no", all.x=T)

tm_shape(ak_map_votes) + tm_fill("percent_dem")

#Drop Alaska from SPDF
pres_2016_co_map_ma <- pres_2016_co_map[pres_2016_co_map$STATEFP != "02",]

#Wanted the U.S. minus map AK joined with AK map. Exported both of the shapefiles, so I drop them in QGIS, where the merge is far esier.
writeOGR(ak_vote_map, ".", "ak_vote_map", driver="ESRI Shapefile" )
writeOGR(pres_2016_co_map_ma, ".", "all_county_votes", driver="ESRI Shapefile")

#Perform merge in QGIS of polygons. Import merged file back to R.

vote_counts_2016_all <- readOGR(".", "nation_county_vote")

                  
