library(data.table)
library(raster)
library(sp)
library(RColorBrewer)
library(leaflet)
library(mapview)
library(spatialEco)
datapath <- getwd()

worldofcovid <- fread(file.path(datapath,"worldcovidcases.csv"))
worldofcovid
worldshp <- shapefile(file.path(datapath, "Datasources/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")) #world SHAPEFILE 
#tail(worldshp@data,100)
setnames(worldofcovid, "Country_code", "ISO2")

#head(worldofcovid)
#set only dems to US ISO code
dems <- worldofcovid[Country == "US-Democrat"]
dems$ISO2 <- "US"
#set only reps to US ISO ode
reps <- worldofcovid[Country == "US-Republican"]
reps$ISO2 <- "US"
#merge into their own separate spatial df
sickdems <- merge(worldshp, dems, by = 'ISO2')
sickreps <- merge(worldshp,reps,  by = 'ISO2')

#attempt to change coordinates :( 
sickreps$LAT <- 25.622
sickreps$LONG <- -88.606

#remove the NAs,, honestly they weren't really being plotted but oh whale
sickdems <- sp.na.omit(sickdems, margin = 1)
sickreps <- sp.na.omit(sickreps, margin = 1)

#sickreps@data
#sickdems@data
#merge the rest of the world
sickworld <- merge(worldshp, worldofcovid, by = 'ISO2')
#plot the world boundaries, if there's a better shapefile maybe find that?
plot(worldshp)
#centroids for world, red, blue
centroids <- getSpPPolygonsLabptSlots(worldshp)
centroidsblue <- getSpPPolygonsLabptSlots(sickdems)
centroidsred <- getSpPPolygonsLabptSlots(sickreps)

#transparent colors and plot!
t_green <- rgb(t(col2rgb(brewer.pal(5, "Greens")[4])), max=255, alpha=0.5*255)
t_red <- rgb(t(col2rgb(brewer.pal(5, "Reds")[4])), max=255, alpha=0.5*255)
t_blue <- rgb(t(col2rgb(brewer.pal(5, "Blues")[4])), max=255, alpha=0.5*255)
points(centroids, pch = 16, col = t_green, cex=sickworld$Cml_cases_pc*100)
points(centroidsblue, pch = 16, col = t_blue, cex=sickdems$Cml_cases_pc*100)
points(centroidsred, pch = 16, col = t_red, cex=sickreps$Cml_cases_pc*100)
