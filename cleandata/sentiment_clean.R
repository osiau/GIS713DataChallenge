# THIS SCRIPT gives you clean data from sentiment analysis of tweets (by states)
# The final clean data object is: sentiment_analysis
# The data is already very condensed - it also seems to only contain one point in time?

library(data.table)
wd <- getwd()

# read

sentiment_analysis <- read.csv(file.path(wd,"Datasources/Sentiment Analysis.csv"),stringsAsFactors=TRUE, header=TRUE)
data_states <- read.csv(file.path(wd,"Datasources/all_names_FIPS.csv"),stringsAsFactors=TRUE)
data_states <- as.data.table(data_states)
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]

# convert to data.table

sentiment_analysis <- as.data.table(sentiment_analysis)

# get the right columns

sentiment_analysis <- sentiment_analysis[,.(geoID = as.factor(formatC(county_fip,
                            width=5,flag="0")),uniq_users,total_post,pos_sntmnt,neu_sntmnt,neg_sntmnt,sntmnt_bal,name)]
sentiment_analysis <- merge(sentiment_analysis,data_states,by="geoID",all.x=TRUE)
sentiment_analysis <- sentiment_analysis[,.(geoID,stateFIPS,countyFIPS,state,
                                  county,uniq_users,total_post,pos_sntmnt,neu_sntmnt,
                                  neg_sntmnt,sntmnt_bal)]
