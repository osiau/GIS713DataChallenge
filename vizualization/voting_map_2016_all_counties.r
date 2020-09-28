library(data.table)
library(stringr)
library(rgdal)
library(tidyr)
library(tmap) #for viewing only
library(rgeos)
library(raster)
library(maptools)
library(sp)
library(tidycensus)
library(leaflet)
library(RColorBrewer)
library(mapview)

wd <- getwd()

#Import shapefile. "..." is the shortcut on my computer via Googel Stream Drive or whatever it's called.
sp1 <- readOGR(file.path(wd, "cleandata/cb_2015_us_county_20m"), "cb_2015_us_county_20m")

#Drop Puerto Rico
sp2 <- sp1[sp1$STATEFP!="72",]

#Read in county vote counts
co_pres16 <- fread(file.path(wd, "cleandata/county_pres_2016.csv"))

co_pres16[, countyFIPS := as.factor(formatC(countyFIPS,width=5,flag="0"))]

co_pres16 <- co_pres16[countyFIPS != "   NA",]

co_pres16 <- co_pres16[!is.na(state_po),]

co_pres16$countyFIPS <- droplevels(co_pres16$countyFIPS)

#Recast from long to wide to have candidate counts/percentages on one row. Can't merge with spatial object otherwise.				  
co_pres16 <- dcast(co_pres16, year+state+state_po+county+countyFIPS+office+totalvotes~candidate, value.var="candidatevotes")

#Necessary for next step
setnames(co_pres16,  "Hillary Clinton" ,"Hillary_Clinton")

#Calculate percentage of votes for democrat
co_pres16[, `:=` (percent_dem = Hillary_Clinton / totalvotes)]

#Calculate percentage of votes for republican
co_pres16[, percent_rep := 1 - percent_dem]

#Binary value of democrat win
co_pres16[, dem_win := ifelse(percent_dem > .50, 1, 0)]
co_pres16[, winning_party := ifelse(percent_dem > .50, "Democrat", "Republican")]

#Subset vote DT to drop AK
co_pres16_no_ak <- co_pres16[state != "Alaska",]

#Subset vote DT to AK
only_ak_vote <- co_pres16[state == "Alaska",]

#Shapefile drop AK
sp2_no_ak <- sp2[sp2$STATEFP != "02",]

#Merge county votes DT with SPDF
pres_2016_map_no_ak <- merge(sp2_no_ak, co_pres16_no_ak, by.x="GEOID", by.y="countyFIPS", all.x = T)

#Import Alaska map of voting districts, which are not the same as counties when it comes to vote counts. "---"is a local directory 
#for me, but this file will be added to Google or Github soone.
ak_vote_shp <- readOGR("Datasources", "2013ProclamationPlan")

#Edit for district numbers only
only_ak_vote$county <- gsub("District ","", only_ak_vote$county)

#Merge vote information with AK SPDF
ak_map_votes <- merge(ak_vote_shp, only_ak_vote, by.x="District_N", by.y="county", all.x=T)

#Transform the ak_map_votes projection to longlat
mepg <- make_EPSG()

longlat <- mepg[mepg$code == "4326", "prj4"]

ak_map_votes <- spTransform(ak_map_votes, longlat)

#Combine U.S. minus AK map with AK map
vote_counts_by_county_2016 <- bind(pres_2016_map_no_ak, ak_map_votes)

#overlay parties
red <- subset(vote_counts_by_county_2016, winning_party == "Republican")
blue <- subset(vote_counts_by_county_2016, winning_party == "Democrat")

#party colors
collist <- c('blue','red')
controlpal <- colorFactor(collist, vote_counts_by_county_2016$winning_party)


labels <- sprintf(
  "<strong>%s</strong><br/> Party: %s",
  vote_counts_by_county_2016$state, vote_counts_by_county_2016$winning_party
) %>% lapply(htmltools::HTML)


# server <- function(input, output, session) {

  # output$mymap <- renderLeaflet({
#m <- 
leaflet(vote_counts_by_county_2016) %>% #begin leaflet map
#all states
addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    fillColor = ~controlpal(winning_party), color = "white", opacity = 1, dashArray = "2", group = "all", 
highlight = highlightOptions( weight = 5, color = "#666", dashArray = "3", fillOpacity = 0.7, bringToFront = TRUE),
label = labels,
labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
#red states
addPolygons(data = red, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    color = ~controlpal(winning_party), group = "red") %>%
#blue states
addPolygons(data = blue, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~controlpal(winning_party), group = "blue") %>%

addLegend("bottomright", pal = controlpal, values = ~winning_party, 
    title = "Winning Party") %>%
addTiles(group = "OSM") %>%
addLayersControl(baseGroups = c("OSM"), 
                overlayGroups = c("red","blue")) #endleaflet
#leafletSizingPolicy(width = "1000px", height= "300px", view.fill = FALSE, browser.fill = FALSE)


mapshot(m, url = paste0(getwd(), "vizualization/general_election_2016.html")) #for exporting

