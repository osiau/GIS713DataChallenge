
library(data.table)
data_dir <- "~/Desktop/dc/Data-Challenge-GIS713/"
setwd(data_dir)

names(unemp_data)[1]<-"geoid"
#mergeunemployment w/ state data
merge(unemp_data,county_data, by="geoid")

#Import covid data cases, deaths at state and county & unemployment
state_data <- fread(file.path(data_dir, "regression supplies/state_votes_and_covid.csv"))
county_data <- fread(file.path(data_dir, "regression supplies/county_votes_and_covid.csv"))
unemp_data <- readRDS("~/Desktop/dc/Data-Challenge-GIS713/regression supplies/unemployment_regres.RDS")

#box plots - covid case data blue vs. red (county & state end point)
boxplot(state_data$total_cases_pc~state_data$democrat, 
        xlab="Party Affliation", ylab="Total cases per capita", 
        main="State-Level", col=c("red","blue"))

boxplot(county_data$total_cases_pc~county_data$democrat, 
        xlab="Party Affliation", ylab="Total cases per capita", 
        main="County-Level", col=c("red","blue"))

boxplot(state_data$total_deaths_pc~state_data$democrat, 
        xlab="Party Affliation", ylab="Total cases per capita", 
        main="State-Level", col=c("red","blue"))

boxplot(county_data$total_deaths_pc~county_data$democrat, 
        xlab="Party Affliation", ylab="Total cases per capita", 
        main="County-Level", col=c("red","blue"))

boxplot(unemp_data$increase~state_data$democrat, 
        xlab="Party Affliation", ylab="Unemployment", 
        main="State-Level", col=c("red","blue"))

county_data$total_cases_pc_adj<-county_data$total_cases_pc + 0.000001
county_data$total_deaths_pc_adj<-county_data$total_deaths_pc + 0.000001
##lm
m0 <- lm(total_cases_pc~1, data=state_data)
m1 <- lm(total_cases_pc~democrat, data=state_data)
m2 <- lm(total_deaths_pc~democrat, data=state_data)
m00 <- lm(total_cases_pc~1, data=county_data)
m3 <- lm(total_cases_pc~democrat, data=county_data)
m4 <- lm(total_deaths_pc~democrat, data=county_data)

#beta



m5 <- betareg(total_cases_pc ~ democrat, data=state_data)
m6 <- betareg(total_deaths_pc ~ democrat, data=state_data)
m7 <- betareg(total_cases_pc_adj ~ democrat, data=county_data)
m8 <- betareg(total_deaths_pc_adj ~ democrat, data=county_data)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

##check Q-Q plots and Cook's distance
plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(m5)
plot(m6)
plot(m7)
plot(m8)

#Run AIC

AIC(m0,m1,m2,m5,m6)
AIC(m00,m3,m4,m7,m8)

