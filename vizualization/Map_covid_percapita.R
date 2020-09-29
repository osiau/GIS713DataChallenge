library(sp)
library(raster)
library(rgdal)
library(spatstat)
library(GISTools)
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(usmap)
library(RColorBrewer)

#prelim
countyshp = shapefile("cb_2015_us_county_20m.shp")
countyshp = subset(countyshp, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")
plot(countyshp)
fips = read.table("county_pres_Result2016.txt", header=TRUE, fill = TRUE, sep = ",", dec = ".")
fips = as.data.table(fips)
# fips = fips[,.(countyFIPS = as.factor(formatC(countyFIPS,width=5,flag="0")))]
fips$countyFIPS = fips[,.(countyFIPS = as.factor(formatC(countyFIPS,width=5,flag="0")))]
NAvalues = fips[is.na(fips$countyFIPS),]

#Assign red/blue in new column based on winning party votes
fips$Color = ifelse(fips$per_dem > fips$per_gop, "blue",ifelse(fips$per_dem< fips$per_gop, "red", "grey"))
keys = fips$countyFIPS
fips = fips[,c(12,13)]



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




#Matching fips columns based on covid data and shapefile
fips1 = as.data.frame(unique(covid_cases_l$countyFIPS))
df = data.frame()

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


#Create Maps
#Jan Map
shp_jan = countyshp
df_jan =  df[month(df$dates) == 1,1:8]
df_jan = as.data.table(df_jan)
df_jan$countyFIPS = df_jan[, as.factor(formatC(df_jan$countyFIPS,width=5,flag="0"))]
shp_jan@data = merge(shp_jan@data,df_jan, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_jan@data$covid_per_cap = shp_jan@data$clm_d_confirmed/shp_jan@data$population
range(shp_jan@data$covid_per_cap)
x=shp_jan@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_jan@data$Size = ifelse(0<= x & x <.0000002,.6, ifelse(.0000002<= x & x <.0000004,1,ifelse(.0000004<= x & x <.0000007,1.5,ifelse(.0000007<= x & x <.000001,1.2,2.5))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2,  main="Covid Cases per capita in U.S. Counties Jan 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_jan@data$Size, col=shp_jan@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"

#Feb Map
shp_feb = countyshp
df_feb =  df[month(df$dates) == 2,1:8]
df_feb = as.data.table(df_feb)
df_feb$countyFIPS = df_feb[, as.factor(formatC(df_feb$countyFIPS,width=5,flag="0"))]
shp_feb@data = merge(shp_feb@data,df_feb, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_feb@data$covid_per_cap = shp_feb@data$clm_d_confirmed/shp_feb@data$population
range(shp_feb@data$covid_per_cap)
x=shp_feb@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_feb@data$Size = ifelse(0<= x & x <.000001,.6, ifelse(.000001<= x & x <.000002,1,ifelse(.000002<= x & x <.000004,1.5,ifelse(.000004<= x & x <.000008,1.2,ifelse(.000008<= x & x <.00001,1.6,ifelse(.00001<= x & x <.00003,1.6,2.5))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Feb 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_feb@data$Size, col=shp_feb@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n", pt.cex = )#"bottomleft"


#March Map
shp_mar = countyshp
df_mar =  df[month(df$dates) == 3,1:8]
df_mar = as.data.table(df_mar)
df_mar$countyFIPS = df_mar[, as.factor(formatC(df_mar$countyFIPS,width=5,flag="0"))]
shp_mar@data = merge(shp_mar@data,df_mar, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_mar@data$covid_per_cap = shp_mar@data$clm_d_confirmed/shp_mar@data$population
range(shp_mar@data$covid_per_cap)
x=shp_mar@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")
#val_range <- round(seq(min(shp_feb@data$covid_per_cap),max(shp_feb@data$covid_per_cap),length.out=2))


shp_mar@data$Size = ifelse(0<= x & x <.002,.6, ifelse(.002<= x & x <.004,1,ifelse(.004<= x & x <.007,1.5,ifelse(.007<= x & x <.01,1.2,2.5))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Mar 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_mar@data$Size, col=shp_mar@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"
#legend(-125.85072 ,27.19325, title = "Volume of Confirmed Covid Cases", legend = as.character(val_range), col = c("blue"), pch=16,pt.cex=c(min(shp_mar@data$Size),max(shp_mar@data$Size)))
#title("Covid Cases per capita in U.S. Counties Mar 2020", line = -2)


#Apr Map
shp_apr = countyshp
df_apr =  df[month(df$dates) == 4,1:8]
df_apr = as.data.table(df_apr)
df_apr$countyFIPS = df_apr[, as.factor(formatC(df_apr$countyFIPS,width=5,flag="0"))]
shp_apr@data = merge(shp_apr@data,df_apr, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_apr@data$covid_per_cap = shp_apr@data$clm_d_confirmed/shp_apr@data$population
range(shp_apr@data$covid_per_cap)
x=shp_apr@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_apr@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Apr 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_apr@data$Size, col=shp_apr@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"


#May Map
shp_may = countyshp
df_may =  df[month(df$dates) == 5,1:8]
df_may = as.data.table(df_may)
df_may$countyFIPS = df_may[, as.factor(formatC(df_may$countyFIPS,width=5,flag="0"))]
shp_may@data = merge(shp_may@data,df_may, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_may@data$covid_per_cap = shp_may@data$clm_d_confirmed/shp_may@data$population
range(shp_may@data$covid_per_cap)
x=shp_may@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_may@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties May 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_may@data$Size, col=shp_may@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"




#JUN Map
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

shp_jun@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))

# col = brewer.pal(4, "Reds")
# # Define colour pallete
# pal = colorRampPalette(c("blue", "red"))
# # Use the following line with RColorBrewer
# pal = colorRampPalette(col)
# palette(c("red","blue","grey"))
# palette(adjustcolor(palette(),alpha.f=0.7))
# #plot(countyshp)
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Jun 2020")
points(centroids, cex = shp_jun@data$Size, col=shp_may@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"


#Jul Map
shp_jul = countyshp
df_jul =  df[month(df$dates) == 7,1:8]
df_jul = as.data.table(df_jul)
df_jul$countyFIPS = df_jul[, as.factor(formatC(df_jul$countyFIPS,width=5,flag="0"))]
shp_jul@data = merge(shp_jul@data,df_jul, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_jul@data$covid_per_cap = shp_jul@data$clm_d_confirmed/shp_jul@data$population
range(shp_jul@data$covid_per_cap)
x=shp_jul@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_jul@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Jul 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_jul@data$Size, col=shp_jul@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"



#Aug Map
shp_aug = countyshp
df_aug =  df[month(df$dates) == 8,1:8]
df_aug = as.data.table(df_aug)
df_aug$countyFIPS = df_aug[, as.factor(formatC(df_aug$countyFIPS,width=5,flag="0"))]
shp_aug@data = merge(shp_aug@data,df_aug, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_aug@data$covid_per_cap = shp_aug@data$clm_d_confirmed/shp_aug@data$population
range(shp_aug@data$covid_per_cap)
x=shp_aug@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_aug@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Aug 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_aug@data$Size, col=shp_aug@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"


#Sept map
shp_sep = countyshp
df_sep =  df[month(df$dates) == 9,1:8]
df_sep = as.data.table(df_sep)
df_sep$countyFIPS = df_sep[, as.factor(formatC(df_sep$countyFIPS,width=5,flag="0"))]
shp_sep@data = merge(shp_sep@data,df_sep, by.x = "GEOID", by.y = "countyFIPS", all.x = TRUE)
shp_sep@data$covid_per_cap = shp_sep@data$clm_d_confirmed/shp_sep@data$population
range(shp_sep@data$covid_per_cap)
x=shp_sep@data$covid_per_cap
us_states = shapefile("cb_2018_us_state_20m.shp")
us_states = subset(us_states, STATEFP != "15" &  STATEFP != "66" & STATEFP != "02" &  STATEFP != "72" &  STATEFP != "60" &  STATEFP != "69" &  STATEFP != "78" &  STATEFP != "99")

shp_sep@data$Size = ifelse(0<= x & x <.005,.6,ifelse(.005<= x & x <.01,1, ifelse(.01<= x & x <.02,1.2,ifelse(.02<= x & x <.03,1.5,ifelse(.03<= x & x <.04,1.75,ifelse(.04<= x & x <.06,2,ifelse(.06<= x & x <.08,2.25,ifelse(.08<= x & x <.1,2.5,3))))))))
#plot(countyshp)
plot(us_states, lty=1, lwd=2, main="Covid Cases per capita in U.S. Counties Sept 2020")
# plot_usmap(labels=TRUE, lty=1, lwd=2, add=T)
points(centroids, cex = shp_sep@data$Size, col=shp_sep@data$Color, pch = 16)#, add=T)
legend(-125.85072 ,31.19325, legend=c("Rep", "Dem"), pch=c(16,16), col=c("red", "blue"), ncol=2, bg="white", cex=1.2, bty="n")#"bottomleft"







