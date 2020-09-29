library(data.table)

"If you take the blue states out, we're at a level that I don't think anybody 
in the world would be at. We're really at a very low level. But some of the 
states, they were blue states and blue-state-managed."

#worldcovid by 2016 election results - state
path_to_data<-"~/Desktop/dc/Data-Challenge-GIS713/"
setwd("~/Desktop/dc/Data-Challenge-GIS713/")
state_data <- fread(file.path(path_to_data, "regression supplies/state_votes_and_covid.csv"))
worldstats_covid19 <- fread(file.path(path_to_data, "Datasources/WHO-COVID-19-global-data.csv"))
worldpop19 <- fread(file.path(path_to_data, "Datasources/WPP2019_TotalPopulation.csv"))


state_data<-as.data.table(state_data)

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


reps_state_data<-state_data[democrat != 1,]
reps_state_data[,sum(total_cases)]
reps_state_data[,sum(total_deaths)]
reps_state_data[,sum(population)]
reps<-data.frame("US-Republican",4113802, "ZZ",93691, 186790.1)
names(reps)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")


dems_state_data<-state_data[democrat==1,]
dems_state_data[,sum(total_cases)]
dems_state_data[,sum(total_deaths)]
dems_state_data[,sum(population)]
dems<-data.frame("US-Democrat",2605710, "ZZ",105445, 141449.4)
names(dems)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")

worldcasespop<-rbind(reps,worldcasespop)
worldcasespop<-rbind(dems,worldcasespop)
worldcasespop_state<-as.data.table(worldcasespop)
worldcasespop_state[,Cml_cases_pc:=(Cumulative_cases/(PopTotal*1000))]
worldcasespop_state[,Cml_deaths_pc:=(Cumulative_deaths/(PopTotal*1000))]


#world COVID19 using state governor party affliation - state level covid data
path_to_data<-"~/Desktop/dc/Data-Challenge-GIS713/"
setwd("~/Desktop/dc/Data-Challenge-GIS713/")
state_data <- fread(file.path(path_to_data, "regression supplies/state_votes_and_covid.csv"))
worldstats_covid19 <- fread(file.path(path_to_data, "Datasources/WHO-COVID-19-global-data.csv"))
worldpop19 <- fread(file.path(path_to_data, "Datasources/WPP2019_TotalPopulation.csv"))
state_gov<- fread(file.path(path_to_data, "Datasources/us-governors.csv"))

state_data<-as.data.table(state_data)
state_gov<-as.data.table(state_gov)
state_gov<-state_gov[,.(state_code,party)]
names(state_gov)[1]<-"state_po"
state_data_wgov<-merge(state_gov,state_data, by="state_po")
state_data_wgov<-as.data.table(state_data_wgov)
state_data_wgov<-state_data_wgov[,.(party,
                                    total_deaths,
                                    total_cases,total_cases_pc,
                                    total_deaths_pc, population)]


worldstats_covid19<-as.data.table(worldstats_covid19)
total_cases_deaths<-worldstats_covid19[worldstats_covid19$Date_reported == "2020-09-09", 
                                       .(Cumulative_cases, Country,Country_code, Cumulative_deaths)]

worldpop19<-as.data.table(worldpop19)
worldpop19<-worldpop19[Time == 2019, .(Location, PopTotal)]
worldpop19<-unique(worldpop19)
total_cases_deaths<-unique(total_cases_deaths)
names(worldpop19)[1]<-"Country"
worldcasespop_stategov<-merge(total_cases_deaths, worldpop19, by="Country")
worldcasespop_stategov<-as.data.table(worldcasespop_stategov)


reps_state_data<-state_data_wgov[party != "democrat",]
reps_state_data[,sum(total_cases)]
reps_state_data[,sum(total_deaths)]
reps_state_data[,sum(population)]
reps<-data.frame("US-Republican",3563451, "ZZ",81600, 152189.6)
names(reps)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")


dems_state_data<-state_data_wgov[party == "democrat",]
dems_state_data[,sum(total_cases)]
dems_state_data[,sum(total_deaths)]
dems_state_data[,sum(population)]
dems<-data.frame("US-Democrat",3141159, "ZZ",116915, 175344.2)
names(dems)<-c("Country","Cumulative_cases","Country_code","Cumulative_deaths","PopTotal")

worldcasespop_stategov<-rbind(reps,worldcasespop_stategov)
worldcasespop_stategov<-rbind(dems,worldcasespop_stategov)
worldcasespop_stategov<-as.data.table(worldcasespop_stategov)
worldcasespop_stategov[,Cml_cases_pc:=(Cumulative_cases/(PopTotal*1000))]
worldcasespop_stategov[,Cml_deaths_pc:=(Cumulative_deaths/(PopTotal*1000))]


#create barplots of US rep & dem vs. world for cases and deaths for state voting
#election results vs. governor party affliation
stcases <- worldcasespop_state[order(-worldcasespop_state$Cumulative_cases),]
head(stcases)
stcases<-stcases[1:10,]
stcasesnames<-stcases$Country
stcasesnames[1]<-"United States"
stcasesnames[6]<-"Russia"

stdeaths <- worldcasespop_state[order(-worldcasespop_state$Cumulative_deaths),]
head(stdeaths)
stdeaths<-stdeaths[1:10,]
stdeathsnames<-stdeaths$Country
stdeathsnames[1]<-"United States"

stgcases <- worldcasespop_stategov[order(-worldcasespop_stategov$Cumulative_cases),]
head(stgcases)
stgcases<-stgcases[1:10,]
stgcasesnames<-stgcases$Country
stgcasesnames[1]<-"United States"
stgcasesnames[6]<-"Russia"

stgdeaths <- worldcasespop_stategov[order(-worldcasespop_stategov$Cumulative_deaths),]
head(stgdeaths)
stgdeaths<-stgdeaths[1:10,]
stgdeathsnames<-stgdeaths$Country
stgdeathsnames[1]<-"United States"

png(filename="worldplots.png", height=6.5, width=6.5, units="in", res=600)
par(mfrow=c(2,2),mai = c(0.75, 0.5, 0.5, 0.1), oma=c(3,0,0,0))
barplot(stcases$Cumulative_cases/1000000,
        main = "",
        xlab = "",
        col = c("#8073ac","#e0e0e0","#e0e0e0","#b2182b","#2166ac","#e0e0e0",
                "#e0e0e0","#e0e0e0","#e0e0e0","#e0e0e0"), 
        ylab= "Total No. of Cases (millions)", names.arg=stcasesnames, las=2, ylim=c(0,7))
text(x=7,y=5,cex=0.4, label="Note: US-Rep. vs. US-Dem. cases based on 2016 election results")

barplot(stdeaths$Cumulative_deaths/1000,
        main = "",
        xlab = "",
        col = c("#8073ac","#e0e0e0","#2166ac","#b2182b","#e0e0e0","#e0e0e0",
                "#e0e0e0","#e0e0e0","#e0e0e0","#e0e0e0"), 
        ylab= "Total No. of Deaths (thousands)", names.arg=stdeathsnames, las=2, ylim=c(0,200))
text(x=7,y=150,cex=0.4, label="Note: US-Rep. vs. US-Dem. deaths based on 2016 election results")


barplot(stgcases$Cumulative_cases/1000000,
        main = "",
        xlab = "",
        col = c("#8073ac","#e0e0e0","#e0e0e0","#b2182b","#2166ac","#e0e0e0",
                "#e0e0e0","#e0e0e0","#e0e0e0","#e0e0e0"), 
        ylab= "Total No. of Cases (millions)", names.arg=stgcasesnames, las=2,ylim=c(0,7))
text(x=7,y=5,cex=0.4, label="Note: US-Rep. vs. US-Dem. cases based party of state governor")


barplot(stgdeaths$Cumulative_deaths/1000,
        main = "",
        xlab = "",
        col = c("#8073ac","#e0e0e0","#2166ac","#b2182b","#e0e0e0","#e0e0e0",
                "#e0e0e0","#e0e0e0","#e0e0e0","#e0e0e0"), 
        ylab= "Total No. of Deaths (thousands)", names.arg=stgdeathsnames, las=2, ylim=c(0,200))
text(x=7,y=150,cex=0.4, label="Note: US-Rep. vs. US-Dem. deaths based party of state governor")

title("Top 20 (out of 232 countries) Total Cases & Deaths", line = -1, outer = TRUE, cex=0.8)
dev.off()

