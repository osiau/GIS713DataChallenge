# THIS SCRIPT gives you clean election data
# The final clean data is called "county_pres_2016.csv"

county_pres <- fread(file.path(data_dir, "countypres_2000-2016.csv"))
county_pres_2016 <- county_pres[year == 2016 & (party == "democrat" |  party == "republican")]

# Replace "democrat" by 1 (Yes) and "republican" by 0 (No)
county_pres_2016[party == "democrat", party := "1"]
county_pres_2016[party == "republican", party := "0"]

# Change FIPS name to countyFIPS
setnames(county_pres_2016, "FIPS", "countyFIPS")

write.csv(county_pres_2016, "county_pres_2016.csv")