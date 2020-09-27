data_dir <- ""
state_pres <- fread(file.path(data_dir, "statepres_1976-2016.csv"))
state_pres_2016 <- state_pres[year == 2016,]
setnames(state_pres_2016, "state_fips", "stateFIPS") # Rename for same FIPS variable name

# Create cols for vote percent and binary 1/0 for dem/not dem
state_pres_2016 <- state_pres_2016[, `:=`(vote_pct = candidatevotes / totalvotes, democrat = ifelse(party == "democrat", 1, 0))]

# Export to csv
write.csv(state_pres_2016, "state_pres_2016.csv")
