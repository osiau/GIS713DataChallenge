library(data.table)
library(raster)
library(sp)
library(RColorBrewer)
library(leaflet)
library(mapview)
datapath <- getwd()

usshp <- shapefile(file.path(datapath, "Datasources/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")) #us states SHAPEFILE 
jobloss <- fread(file.path(datapath,"vizualization/stateunemployment_longrate.csv"))
votes2016 <- fread(file.path(datapath,"2016votepartywinner.csv"),select=c('state_abbr','partywinner'))
#remove puerto rico :(
usshp <- subset(usshp, GEOID != '72')
votes2016 <- subset(votes2016, state_abbr != "puerto Rico")
jobloss<-subset(jobloss, state != "puerto Rico")

setnames(votes2016,'state_abbr','STUSPS')

usshpwinners <- merge(usshp,votes2016, by='STUSPS')

d <- as.Date(jobloss$month,format='%m/%d/%y')

jobloss$month <- format(d, '%m')
jobloss$month[jobloss$month == '03'] <- "March"
jobloss$month[jobloss$month == '04'] <- "April"
setnames(jobloss,'state','NAME')



firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
jobloss$NAME <- firstup(jobloss$NAME)
alljobloss <-jobloss
jobloss <- subset(jobloss, month=="April")
#jobloss
#usshp
sjl <- merge(usshpwinners,jobloss, by='NAME')
#sjl@data
#head(sjl)
red <- subset(sjl, partywinner %in% "red")
blue <- subset(sjl, partywinner %in% "blue")

bqpal <- colorQuantile("Blues", alljobloss$rate, n=9)
rqpal <- colorQuantile("Reds", alljobloss$rate, n=9)
#bbinpal <- colorBin("Blues", alljobloss$rate, 5, pretty = TRUE)
#rbinpal <- colorBin("Reds", alljobloss$rate,  5, pretty = TRUE)

m <- leaflet(sjl) %>% #begin leaflet map
    #blue states
    addPolygons(data = blue, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
    fillColor = ~bqpal(rate),  group = "blue") %>%
    #red states
    addPolygons(data = red, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1, 
        fillColor = ~rqpal(rate), group = "red") %>%
    addLegend("bottomright", pal = bqpal, values = ~rate, title = "Blue State Unemployment Rate (Apr 2020)", opacity = 1, 
    labFormat =  function(type, cuts, p) { 
        n = length(cuts) 
        cuts[n] = "More Job Loss" 
        for (i in 2:(n-1)){cuts[i] = ""} 
        cuts[1] = "Less Job Loss" 
        paste0(cuts[-n], cuts[-1])}) %>%
    addLegend("bottomright", pal = rqpal, values = ~rate, title = "Red State Unemployment Rate (Apr 2020)", opacity = 1, 
    labFormat =  function(type, cuts, p) { 
        n = length(cuts) 
        cuts[n] = "More Job Loss" 
        for (i in 2:(n-1)){cuts[i] = ""} 
        cuts[1] = paste0("Less Job Loss ", min(red$rate), '%')
        paste0(cuts[-n], cuts[-1])}) %>%
    addLayersControl(#baseGroups = c("OSM"), 
                   overlayGroups = c("red","blue")) #endleaflet



#mapshot(m, file =  paste0(getwd(),"/Rplot.png"))
#savewidget first then webshot might have to fitbounds 
#webshot("sn.html", "simpleNetwork.png")
#mapshot(m, url = paste0(getwd(), "/uemap.html")) #for exporting
