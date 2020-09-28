"If you take the blue states out, we're at a level that I don't think anybody 
in the world would be at. We're really at a very low level. But some of the 
states, they were blue states and blue-state-managed."


library(data.table)

path_to_data<-"~/Desktop/dc/Data-Challenge-GIS713/"
setwd("~/Desktop/dc/Data-Challenge-GIS713/")
county_data <- fread(file.path(path_to_data, "regression supplies/county_votes_and_covid.csv"))
worldstats_covid19 <- fread(file.path(path_to_data, "Datasources/WHO-COVID-19-global-data.csv"))
worldpop19 <- fread(file.path(path_to_data, "Datasources/WPP2019_TotalPopulation.csv"))


county_data<-as.data.table(county_data)


worldstats_covid19<-as.data.table(worldstats_covid19)
total_cases_deaths<-worldstats_covid19[worldstats_covid19$Date_reported == "2020-09-09", 
                               .(Cumulative_cases, Country,Country_code, Cumulative_deaths)]

worldpop19<-as.data.table(worldpop19)
worldpop19<-worldpop19[Time == 2019, .(Location, PopTotal)]
worldpop19<-unique(worldpop19)
total_cases_deaths<-unique(total_cases_deaths)
names(worldpop19)[1]<-"Country"
worldcasespop<-merge(total_cases_deaths, worldpop19, by="Country")
worldcasespop<-as.data.table(worldcasespop)


reps_county_data<-county_data[democrat != 1,]
reps_county_data[,sum(total_cases)]
reps_county_data[,sum(total_deaths)]
reps_county_data[,sum(population)]
reps<-data.frame("US-Republican",2768073, "ZZ",63560, 149255.8)
names(reps)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")


dems_county_data<-county_data[democrat==1,]
dems_county_data[,sum(total_cases)]
dems_county_data[,sum(total_deaths)]
dems_county_data[,sum(population)]
dems<-data.frame("US-Democrat",3907643, "ZZ",135081, 178534.9)
names(dems)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")

worldcasespop<-rbind(reps,worldcasespop)
worldcasespop<-rbind(dems,worldcasespop)
worldcasespop<-as.data.table(worldcasespop)
worldcasespop[,Cml_cases_pc:=(Cumulative_cases/(PopTotal*1000))]
worldcasespop[,Cml_deaths_pc:=(Cumulative_deaths/(PopTotal*1000))]

     