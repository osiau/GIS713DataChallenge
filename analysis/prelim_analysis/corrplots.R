library(data.table)
library(corrplot)

data_dir <- "~/Desktop/corrplots/data"
setwd(data_dir)

#Counties
#Import covid data cases, deaths at state and county & unemployment####
county_data<-read.csv("county_var_AQS_Resilience_Hospital_Census.csv")
county_data$democrat <- ifelse(county_data$vote_pct>=0,"1","0")
county_data$vote_pct <- ifelse(county_data$vote_pct<0,
                               (1-(county_data$vote_pct*-1)),county_data$vote_pct)

#boxplots counties####
png(filename="cntylvlcovid_sumplots", width = 6, height = 8, units = "in", 
    res = 600)
par(mfrow=c(3,2))
boxplot(county_data$total_cases_pc~county_data$democrat, 
        xlab="", ylab="Total cases per capita", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(county_data$total_cases_pc~county_data$vote_pct, 
        xlab="% Votes Clinton", ylab="Total cases per capita", 
        main="")
title("County-Level Outcomes vs. Political Affliation", line = -3, outer = TRUE)
m1<-lm(county_data$total_cases_pc~county_data$vote_pct)
abline(m1, col=c("#fee090"), lwd=1.5)
text(x = 0.15,y=0.07, labels="R^2: 0.03", cex=0.6)
text(x = 0.15,y=0.06, labels="p:<0.01", cex=0.6)
summary(m1)
boxplot(county_data$total_deaths_pc~county_data$democrat, 
        xlab="", ylab="Total deaths per capita", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(county_data$total_deaths_pc~county_data$vote_pct, 
     xlab="% Votes Clinton", ylab="Total deaths per capita", 
     main="")
m2<-lm(county_data$total_deaths_pc~county_data$vote_pct)
abline(m2, col=c("#fee090"), lwd=1.5)
text(x = 0.15,y=0.0030, labels="R^2: 0.12", cex=0.6)
text(x = 0.15,y=0.0025, labels="p: <0.01", cex=0.6)
summary(m2)

boxplot(county_data$unemploy~county_data$democrat, 
        xlab="", ylab="Unemployment Rate", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(county_data$unemploy~county_data$vote_pct, 
     xlab="% Votes Clinton", ylab="Unemployment Rate", 
     main="")
m3<-lm(county_data$unemploy~county_data$vote_pct)
abline(m3, col=c("#fee090"), lwd=1.5)
text(x = 0.15,y=25, labels="R^2: 0.00", cex=0.6)
text(x = 0.15,y=20, labels="p: 0.35", cex=0.6)
summary(m3)
dev.off()


#correlation plots - county level####
county_data<-as.data.table(county_data)
county_data_c<-county_data[,.(total_cases_pc, total_deaths_pc, unemploy,
                              dem_vote_pct , max.aqi , median.aqi, 
                              popn_low_risk, popn_med_risk,popn_high_risk, 
                              beds_county, hospitals_county, median_age, 
                              med_income, frac_white, frac_black, frac_asian, 
                              frac_pacislander, frac_otherrace, frac_under18, 
                              frac_over65, frac_insured, frac_pubtransport)]

names(county_data_c)[1]<-"cases"
names(county_data_c)[2]<-"deaths"
names(county_data_c)[3]<-"unemployment"
names(county_data_c)[4]<-"%_dem_vote"
county_data_cor<-(cor(county_data_c, use="na.or.complete"))


png(filename="countylevelcorr.png", width=6, height=6, unit="in",res=600)
corrplot(county_data_cor, type="upper", method="circle")
title("Correlations of County-Level Variables", line = -1, outer = TRUE, cex=0.8)
dev.off()

#re-run above but for states
#Import covid data cases, deaths at state and county & unemployment####


state_d <- readRDS("state_dependent_vars.RDS")
state_i <- readRDS("state_independent_vars.RDS")
state_data<-merge(state_d,state_i, by.x="stateFIPS",by.y="stateFIPS")
state_data$democrat <- ifelse(state_data$vote_pct>=0,"1","0")
state_data$votes_dem <- ifelse(state_data$vote_pct<0,
                               (1-state_data$vote_pct*-1),state_data$vote_pct)

#boxplots counties####
png(filename="statelvlcovid_sumplots", width = 6, height = 8, units = "in", 
    res = 600)
par(mfrow=c(3,2))
boxplot(state_data$total_cases_pc~state_data$democrat, 
        xlab="", ylab="Total cases per capita", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(state_data$total_cases_pc~state_data$votes_dem, 
     xlab="% Votes Clinton", ylab="Total cases per capita", 
     main="")
title("State-Level Outcomes vs. Political Affliation", line = -3, outer = TRUE)
m4<-lm(state_data$total_cases_pc~state_data$votes_dem)
abline(m4, col=c("#fee090"), lwd=1.5)
text(x = 0.84,y=0.03, labels="R^2: 0.01", cex=0.6)
text(x = 0.84,y=0.027, labels="p:<0.79", cex=0.6)
summary(m4)
boxplot(state_data$total_deaths_pc~state_data$democrat, 
        xlab="", ylab="Total deaths per capita", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(state_data$total_deaths_pc~state_data$votes_dem, 
     xlab="% Votes Clinton", ylab="Total deaths per capita", 
     main="")
m5<-lm(state_data$total_deaths_pc~state_data$votes_dem)
abline(m5, col=c("#fee090"), lwd=1.5)
text(x = 0.84,y=0.0017, labels="R^2: 0.12", cex=0.6)
text(x = 0.84,y=0.0014, labels="p: <0.01", cex=0.6)
summary(m5)

boxplot(state_data$unemploy~state_data$democrat, 
        xlab="", ylab="Unemployment Rate", 
        main="", col=c("#a50026","#313695"), names=c("Voted Trump","Voted Clinton"))
plot(state_data$unemploy~state_data$votes_dem, 
     xlab="% Votes Clinton", ylab="Unemployment Rate", 
     main="")
m6<-lm(state_data$unemploy~state_data$votes_dem)
abline(m6, col=c("#fee090"), lwd=1.5)
text(x = 0.83,y=25, labels="R^2: 0.09", cex=0.6)
text(x = 0.83,y=22, labels="p: <0.01", cex=0.6)
summary(m6)
dev.off()


#correlation plots - state level####
state_data<-as.data.table(state_data)
state_data_c<-state_data[,.(total_cases_pc, 
                            total_deaths_pc, unemploy,vote_pct, 
                            beds_state, hospitals_state,
                            aqi_2019,median_age,med_income,frac_white,
                            frac_poc=1-frac_white,frac_black,frac_amindian,
                            frac_amindian,frac_asian,frac_pacislander,
                            frac_otherrace,frac_under18,frac_over65,
                            frac_insured,frac_pubtransport,tourism,
                            popn_high_risk,popn_med_risk)]

names(state_data_c)[1]<-"cases"
names(state_data_c)[2]<-"deaths"
names(state_data_c)[3]<-"unemployment"
names(state_data_c)[4]<-"%_dem_vote"
state_data_cor<-(cor(state_data_c, use="na.or.complete"))


png(filename="statelevelcorr.png", width=6, height=6, unit="in",res=600)
corrplot(state_data_cor, type="upper", method="circle")
title("Correlations of State-Level Variables", line = -1, outer = TRUE, cex=0.8)
dev.off()
