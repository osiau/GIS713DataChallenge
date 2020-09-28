library(data.table)
library(stringr)
library(rgdal)
library(tidyr)
library(tmap) #for viewing only
library(rgeos)

wd <- getwd()

#Import shapefile. "..." is the shortcut on my computer via Googel Stream Drive or whatever it's called.
sp1 <- readOGR(file.path(wd, "cb_2015_us_county_20m"), "cb_2015_us_county_20m")

#Drop Puerto Rico
sp2 <- sp1[sp1$STATEFP!="72",]

#Read in county vote counts
co_pres16 <- fread(file.path(wd, "county_pres_2016.csv"))

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

#Merge county votes DT with SPDF
pres_2016_co_map <- merge(sp2, co_pres16, by.x="GEOID", by.y="countyFIPS", all.x = T)

setwd("..")
#Import Alaska map of voting districts, which are not the same as counties when it comes to vote counts. "---"is a local directory 
#for me, but this file will be added to Google or Github soone.
alp1 <- readOGR("Datasources", "2013ProclamationPlan")

mepg <- make_EPSG()

#Select for Albers Equal Area for presentation
alberse <- mepg[mepg$code == "5070","prj4"]

#Subset to Alaska votes only
ak_votes <- co_pres16[co_pres16$state=="Alaska",]

#Edit for district numbers only
ak_votes$district_no <- gsub("District ","", ak_votes$county)

#Transform
pres_2016_co_map <- spTransform(pres_2016_co_map, alberse)

#Transform
ak_vote_map <- spTransform(alp1, alberse)

#Merge vote information with SPDF
ak_map_votes <- merge(ak_vote_map, ak_votes, by.x="District_N", by.y="district_no", all.x=T)

#Drop Alaska from SPDF
pres_2016_co_map_ma <- pres_2016_co_map[pres_2016_co_map$STATEFP != "02",]

#Combine U.S. minus AK map with AK map
vote_counts_by_county_2016 <- bind(pres_2016_co_map_ma, ak_vote_map)


#overlay parties
red <-subset(control, StateControl == "Rep")
blue <-subset(control, StateControl == "Dem")
divide <-subset(control, StateControl == "Divided")
other <- subset(control, StateControl == "NPP")

red <- subset(vote_counts_by_county_2016, dem_win == "0")
blue <- subset(vote_counts_by_county_2016, dem_win == "1")

#party colors
collist <- c('blue','red')
vote_pal <- colorFactor(collist, vote_counts_by_county_2016$dem_win)


labels <- sprintf(
  "<strong>%s</strong><br/> Party: %s",
  vote_counts_by_county_2016$party
) %>% lapply(htmltools::HTML)


# server <- function(input, output, session) {

  # output$mymap <- renderLeaflet({
m <- leaflet(vote_counts_by_county_2016) %>% #begin leaflet map
#all states
addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    fillColor = ~vote_pal(dem_win), color = "white", opacity = 1, dashArray = "3", group = "all", 
highlight = highlightOptions( weight = 5, color = "#666", dashArray = "3", fillOpacity = 0.7, bringToFront = TRUE),
label = labels,
labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
#red states
addPolygons(data = red, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    color = ~vote_pal(dem_win), group = "red") %>%
#blue states
addPolygons(data = blue, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~vote_pal(dem_win), group = "blue") %>%
#divided
addPolygons(data = divide, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~vote_pal(dem_win),  group = "divided") %>%
#other
addPolygons(data = other, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~vote_pal(dem_win),  group = "other") %>%

addLegend("bottomright", pal = vote_pal, values = ~dem_win, 
    title = "Winning Party") %>%
addTiles(group = "OSM") %>%
addLayersControl(baseGroups = c("OSM"), 
                overlayGroups = c("all","red","blue","divided","other")) #endleaflet
leafletSizingPolicy(width = "1000px", height= "300px", view.fill = FALSE, browser.fill = FALSE)


mapshot(m, url = paste0(getwd(), "/general_election_2016.html")) #for exporting

