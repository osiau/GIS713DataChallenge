library(data.table)
wd <-getwd()

data_states <- fread(file.path(wd,"cleandata/all_names_FIPS.csv"))
data_states <- data_states[,.(stateFIPS = as.factor(formatC(stateFIPS,width=2,flag="0")), 
                              countyFIPS=as.factor(formatC(countyFIPS,width=3,flag="0")), 
                              geoID=as.factor(formatC(geoID,width=5,flag="0")),county,state)]
state_abvr <- fread(file.path(wd,"Datasources/state_abvr.csv"),stringsAsFactors=TRUE)
state_abvr <- state_abvr[,.(state=tolower(state),abvr=tolower(abvr))]

votes <- fread(file.path(wd,"cleandata/county_pres_Result2016.csv"),stringsAsFactors=TRUE, header=TRUE)

votes <- votes[,.(votes_dem=sum(votes_dem),votes_gop=sum(votes_gop),total_votes=sum(total_votes)),by=state_abbr]
votes
votes$partywinner = ifelse(votes$votes_dem > votes$votes_gop, "blue", ifelse(votes$votes_dem < votes$votes_gop, "red", "grey"))

#data_states
votes
#write.csv(votes,paste0(getwd(), "/2016votepartywinner.csv"), row.names = FALSE)
