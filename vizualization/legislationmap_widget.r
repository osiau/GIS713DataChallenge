library(raster)
library(maptools)
library(data.table)
library(sp)
library(tidycensus)
library(leaflet)
library(RColorBrewer)
library(mapview)

setwd("cleandata")
getwd()
usashp <- shapefile("cb_2015_us_county_20m/cb_2015_us_county_20m.shp") 

LC <- fread("legislative_control.csv", header = TRUE, drop=c("Senateother","Houseother"))
LC[LC == "N/A"] <- NA
setnames(LC, "STATE", "NAME")
control <- merge(usashp,LC,by="NAME") #merged file

#overlay parties
red <-subset(control, StateControl == "Rep")
blue <-subset(control, StateControl == "Dem")
divide <-subset(control, StateControl == "Divided")
other <- subset(control, StateControl == "NPP")
#party colors
collist <- c('blue','yellow','green','red')
controlpal <- colorFactor(collist, control$StateControl)


labels <- sprintf(
  "<strong>%s</strong><br/> Governor: %s",
  control$NAME, control$Gov.Party
) %>% lapply(htmltools::HTML)

# server <- function(input, output, session) {

  # output$mymap <- renderLeaflet({
m <- leaflet(control) %>% #begin leaflet map
#all states
addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    fillColor = ~controlpal(StateControl), color = "white", opacity = 1, dashArray = "3", group = "all", 
highlight = highlightOptions( weight = 5, color = "#666", dashArray = "3", fillOpacity = 0.7, bringToFront = TRUE),
label = labels,
labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
#red states
addPolygons(data = red, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    color = ~controlpal(StateControl), group = "red") %>%
#blue states
addPolygons(data = blue, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~controlpal(StateControl), group = "blue") %>%
#divided
addPolygons(data = divide, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~controlpal(StateControl),  group = "divided") %>%
#other
addPolygons(data = other, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
    color = ~controlpal(StateControl),  group = "other") %>%

addLegend("bottomright", pal = controlpal, values = ~StateControl, 
    title = "Party Control") %>%
addTiles(group = "OSM") %>%
addLayersControl(baseGroups = c("OSM"), 
                overlayGroups = c("all","red","blue","divided","other")) #endleaflet
leafletSizingPolicy(width = "1000px", height= "300px", view.fill = FALSE, browser.fill = FALSE)


mapshot(m, url = paste0(getwd(), "/legislative_widget.html")) #for exporting



# saveWidget(pp, file=paste0(getwd(), "/streamgraphBasic.html"))