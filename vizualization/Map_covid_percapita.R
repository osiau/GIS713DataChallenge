library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(tidyverse)
library(GISTools)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)


countyshp = shapefile("cb_2015_us_county_20m.shp")
countyshp = subset(countyshp, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")
plot(countyshp)
fips = read.table("county_pres_Result2016.txt", header=TRUE, fill = TRUE, sep = ",", dec = ".")
fips = as.data.table(fips)
# fips = fips[,.(countyFIPS = as.factor(formatC(countyFIPS,width=5,flag="0")))]
fips$countyFIPS = fips[,.(countyFIPS = as.factor(formatC(countyFIPS,width=5,flag="0")))]
NAvalues = fips[is.na(fips$countyFIPS),]


fips$Color = ifelse(fips$per_dem > fips$per_gop, "blue",ifelse(fips$per_dem< fips$per_gop, "red", "grey"))
keys = fips$countyFIPS

#fips = select(fips$countyFIPS, fips$Color)
# countyshp@data = as.data.table(countyshp@data)

# dt = data.table(countyshp@data, key = countyshp@data$GEOID, fill=TRUE)
fips = fips[,c(12,13)]


# for i in fips$countyFIPS{
# countyshp@data$Color = [match(fips$countyFIPS[1], countyshp@data$GEOID)]
# }
countyshp@data = merge(countyshp@data,fips, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)

# layout(matrix(1,1,2,2), nrow=1)
centroids <- getSpPPolygonsLabptSlots(countyshp)
plot(countyshp)#col = countyshp@data$Color)
points(centroids, pch = 16, col = countyshp@data$Color, cex=.2)


#Get Covid Case data

#data_dir <- "/Users/cyborginhas/Desktop/Datasources/"
#county_shp <- shapefile(file.path(data_dir, "cb_2015_us_county_20m", "cb_2015_us_county_20m.shp"))
# data from "USA FACTS" website: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
covid_cases <- fread( "covid_confirmed_usafacts.csv")
covid_countypop <- fread("covid_county_population_usafacts.csv")


covid_cases <- merge(covid_countypop[, .(countyFIPS, population)], covid_cases,
                     by="countyFIPS")
# covidt = t(covid_cases)

# fix the column name
names(covid_cases)[3] <- "county" # get rid of space in var name
names(covid_cases)[4] <- "state"
# change case count variable names to dates
covid_dates <- as.Date(names(covid_cases)[6:ncol(covid_cases)],
                       format="%m-%d-%y")
#transpose covid cases to calculate daily totals
covid_cases_l<-gather(covid_cases,dates,clm_d_confirmed,6:247)
covid_cases_l$dates<-mdy(covid_cases_l$dates)
#calculate daily counts per county
covid_cases_l<-as.data.table(covid_cases_l)
covid_cases_l<-covid_cases_l[order(county,state,dates),]
covid_cases_l = covid_cases_l %>% group_by(countyFIPS) %>% 
  mutate(d_confirmed = clm_d_confirmed-lag(clm_d_confirmed))
covid_cases_l$county<-tolower(covid_cases_l$county)
covid_cases_l$state<-tolower(covid_cases_l$state)
write.csv(covid_cases_l,"covid_confirmed_clean.csv")


fips1 = as.data.frame(unique(covid_cases_l$countyFIPS))


#bymonth <- aggregate(cbind(Melbourne,Southern,Flagstaff)~month(Date),
#                    data=data,FUN=sum)

df = data.frame()

#out =  setdiff(list(fips1), list(unique(covid_cases_l$countyFIPS)))
# x = intersect(fips1$`unique(covid_cases_l$countyFIPS)`,unique(covid_cases_l$countyFIPS))
# x = as.data.frame(x)


for (i in 1:length(fips1$`unique(covid_cases_l$countyFIPS`)){
  covid = covid_cases_l
#  covid = covid[covid$countyFIPS == fips1[i,]]
  covid = subset(covid, covid$countyFIPS ==fips1$`unique(covid_cases_l$countyFIPS)`[i]) 
 # covid = subset(covid, day(covid$dates)==30 | day(covid$dates)==31) 
  covid =  covid %>% arrange(covid) %>%
    group_by(strftime(covid$dates, "%Y-%m")) %>% #Groups by the yearmonths
    filter(dates == max(dates))
  df = rbind(df,covid)
  }


shp_jan = countyshp
df_jan =  df[month(df$dates) == 1,1:8]

shp_jan@data = merge(shp_jan@data,df_jan, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)





shp_jun = countyshp
df_jun =  df[month(df$dates) == 6,1:8]
df_jun = as.data.table(df_jun)
df_jun$countyFIPS = df_jun[, as.factor(formatC(df_jun$countyFIPS,width=5,flag="0"))]
shp_jun@data = merge(shp_jun@data,df_jun, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_jun@data$covid_per_cap = shp_jun@data$clm_d_confirmed/shp_jun@data$population
range(shp_jun@data$covid_per_cap)
x=shp_jun@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_jun@data$Size = ifelse(0<= x & x <.02,.6, ifelse(.02<= x & x <.04,1,ifelse(.04<= x & x <.07,1.5,ifelse(.07<= x & x <.1,1.2,2.5))))
plot(countyshp)
plot(us_states, lty=1, lwd=3, add=T)
points(centroids, cex = shp_jun@data$Size, col=shp_jun@data$Color, pch = 16)#, add=T)

